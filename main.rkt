#lang scribble/lp

@(require scribble/eval
	  (for-label racket))

@CHUNK[<*>
       (require
	(for-syntax racket)
	(for-syntax "set-values.rkt")
	"set-values.rkt"
	 racket/generator
         anaphoric/acond)
       <define-end-of-generator>
       <local-macros>
       <supporting-functions>
       <return>
       <add-cond-clause>
       <make-hash-generator>
       <all-the-rest>
       (define (macroexpand-1 datum)
	 (syntax->datum (expand-once datum)))
       (provide macroexpand-1 it)
       ]

@section{Introduction}

This is an implementation of Common Lisp's LOOP macro for Racket. The LOOP macro is similar to all of Racket's for/* macros, combined with
Python's @scheme[for] loop, except it's more powerful than either.

Examples:

@racketblock[
	     (define (sift pred? list)
	       (loop for value in list
		     when (pred? value) consing value into gold
		     else consing value into dirt
		     finally (return (values (reverse gold) (reverse dirt)))))
]

@racketinput[(loop for x in '(a b c d e f g)
		   for y from 0
		   when (even? y)
		   collect x)]

→ @racketresult[(a c e g)]


@racketinput[(loop for x in '(a b c d e f g)
		   for y in '(1 2 3 4 5 6 7)
		   with-collection-type hash
		   collect (cons x y))]

→ @racketresult[ #hash((c . 3) (f . 6) (g . 7) (e . 5) (d . 4) (a . 1) (b . 2)) ]

LOOP can also do the job of @scheme[for/and]:

@racketblock[(loop for x in a-list
		   for y in another-list always (and (number? x)
						     (symbol? y)))]

...or @scheme[for/or]:

@racketblock[(loop for x in a-list thereis (symbol? x))]

...or @scheme[for/sum]:

@racketblock[(loop for x in a-list when (integer? (sqrt x)) sum x)]

...or you can convert a list into a hash table:

@racketblock[(loop with collection-type 'hash/immutable
		    for key-value in '((key . val) (key2 . val2))
		    collect key-value)]

...or you can write an old-fashioned while loop:

@racketblock[(loop for line = (read-line socket)
		   while (not (eof-object? line))
		   do
		     (display line)
		     (newline)
		   finally
		     (close-input-port socket))]

The loop macro can also iterate over generators as defined in the  @scheme[racket/generator] package.

@racketblock[(loop for item in (gen)
		   do
		     (displayln item))]

Since @scheme[racket/generator] provides no non-ambiguous way to end a generator, arrange for your generator
to yield the value @scheme[end-of-generator] to terminate the loop, or use an explicit @scheme[return] clause
to exit.

@CHUNK[<define-end-of-generator>
       (define-struct end-of-generator* ())
       (define end-of-generator (make-end-of-generator*))
       (define end-of-generator? end-of-generator*?)
       (provide end-of-generator end-of-generator?)
       ]

@section{Enabling @scheme[return]}

In Common Lisp, the LOOP macro is often used in conjunction with @scheme[return] and @scheme[return-from]. This library defines
@scheme[return] as a macro which invokes a continuation that can be tucked away within this module.

@scheme[return] is defined as a macro and not a function because in Common Lisp, it's legal to do this:

@racketblock[
	     (return (values 1 2 3 4))
	     ]

...where a function would not be able to receive the multiple values. @italic{return} and its hidden continuation are defined as follows:

@CHUNK[<return>
       (define return-cc (make-parameter #f))
       
       (define-syntax return
	 (syntax-rules ()
	   ((_) (return (void)))
	   ((_ value-form)
	    (call-with-values (λ () value-form)
	      (λ all-values
		 (apply (return-cc) all-values))))))
       (define-syntax return-from
	 (syntax-rules ()
	   ((_ block-name value-form)
	    (parameterize ((return-cc block-name))
	       (return value-form)))))
       (provide return return-from)
]

When the LOOP macro is invoked, it sets the @racketresult[return-cc] parameter with its own continuation, which is only an escape continuation.

There is also a @scheme[return] action clause, which is discussed below.

@section{The Main Body}

@subsection{Variables used during macro expansion}

The traditional Scheme way to write anything at all is to define all the variables as arguments
to a recursive loop-function, and to change those variables, you pass @italic{every} variable to the next
iteration of the loop-function, giving new values for the variables that should be different for the next
iteration. At first I began with a design like that (and most of the supporting functions are still written
this way), but as the number of variables grew, it became more
than a little difficult to pass all of them as arguments at every point in the program where the
loop-function was called. Every time a new variable was added, it was necessary to go back and change
@italic{all} the points where recursion took place. A big chunk of the code I wrote this way had to
be deleted and rewritten from scratch.

As a result, I ended up taking most of the variables out of the loop-function's argument list and just changing
them with @scheme[set!]. It may be less "Rackety", but it gets the job done.

The main body of the macro iterates over all the clauses and builds the following variables:

@CHUNK[<expansion-variables>
     (define call-with-cc #'call/ec)
     (define return-continuation #'loop-return)
     (define collection #'collection)
     (define initial-collection #'#f)
     (define count* #'count)
     (define initial-count #'#f)
     (define sum* #'sum)
     (define min #'min)
     (define max  #'max)
     (define reverse? #'reverse?)
     (define reverse-value #'#t)
     (define string-accumulator #'string-accumulator)
     (define initial-string #'#f)
     (define initial-sum #'#f)
     (define and? #f)
     (define conditional-stack '())

     (define collection-type #''list)
     

     ;; Syntax accumulators

     (define prologue #'())
     (define initially-prologue #'())
     (define epilogue #'())
     (define iterations #'())
     (define let-values-defs #'())
     (define current-condition #'())
     (define loop-conditions #'())
     (define loop-preconditions #'())
     (define action-clauses #'())
     (define current-cond-body #'())
     (define body #'())
     (define list-defs #'())
     (define let-defs #'())
     (define gnarled-let-defs #'())
     (define current-gnarled-let-def #'())
]


@itemlist[ @item{@scheme[finally] is the @scheme[finally] clause, which gets executed after everything else. It can be bypassed
			with a @scheme[return] form. Often, a @scheme[return] form is used in a @scheme[finally] clause to
			cause the loop to return loop-local variables.}
	   @item{@scheme[iterations] is a syntax-list of all the variables that must be changed with each iteration of the loop, along with
			a snippet of code that does the change. An example of what might be in this variable is
			@scheme[#'((variable-name (add1 variable-name))
				   (var2 (cdr var2)))] The @scheme[iterations] get executed just as the loop is recursing into the next iteration}
	   @item{@scheme[preconditions] is a list of Boolean forms that determine if the loop body will be executed. Most of these forms
			are @scheme[begin] forms that also iterate something. }
	   @item{@scheme[current-condition] is a list of boolean clauses that is built while processing @scheme[if] clauses.}
	   @item{@scheme[loop-conditions] A syntax-list of conditions that will be combined with the @scheme[and]
			operator to determine if the loop should continue. These are checked just before the @scheme[iterations] are executed.}
	   
	   @item{@scheme[action-clauses] This is a list of action forms that will be combined with the @scheme[current-conditions] if they are
			defined, otherwise they go into the @scheme[body] naked.}
	   @item{@scheme[current-cond-body] This is a list of clauses for a @scheme[cond] form. This
			implements the @scheme[if] and @scheme[else], and @scheme[do] clauses of the loop. The @scheme[current-condition] gets added
			to this list along with code from one or more action clauses.}
	   @item{@scheme[body] is a collection of all action clauses and @scheme[current-cond-body]s to be executed. }
	   @item{@scheme[list-defs] are let-bindings for any lists that are being iterated over using a @scheme[for] clause. They are
			named with the @scheme[(gensym)] function.}
	   @item{@scheme[let-defs] are let-bindings for any variables bound with a @scheme[for] clause.}]

@subsection{Variables representing identifiers}

References to identifiers in the <loop-body> below need to be made from the code in the variables above, and this code is
generated outside of the scope of the <loop-body>. Because of Racket's hygienic macros, the only way to do this is to put
the identifiers themselves into variables that have a wider scope. Some of these identifiers are just the names of variables
within the @scheme[local-loop] block, but others may be changed during macro expansion:

@itemlist[ @item{@scheme[call-with-cc] determines the type of continuation that will be used. This was going to be used
			to implement a @scheme[yield] clause, which would change it from @scheme[call/ec] (the default)
			to @scheme[call/cc], but the @scheme[racket/generator] package already provides a @scheme[yield]
			form that can be used effectively from within the loop macro, so I'll save myself the headache
			of reinventing generators for Racket.}
	   @item{@scheme[initial-count], @scheme[initial-sum], @scheme[initial-string], and @scheme[initial-collection]
			are initial values for collector variables. They default to @scheme[#f] unless the relevant
			clause is used. The @scheme[#f] value is used to control the return value of the loop.}
	   ]

These variables can then be combined to form the loop itself, as it will eventually be expanded.:

@CHUNK[<loop-body>
       #`(let ((#,collection #,initial-collection)
	       (#,count* #,initial-count)
	       (#,sum* #,initial-sum)
	       (#,min #f)
	       (#,max #f)
	       (#,string-accumulator #,initial-string)
	       (#,reverse? #,reverse-value)
	       #,@let-defs
	       #,@list-defs)
	   (#,call-with-cc
	    (λ (#,return-continuation)
	       (parameterize
		((return-cc #,return-continuation))
		(gnarled-let-nest
		 #,gnarled-let-defs
		 (begin
		   #,@prologue
		   #,@initially-prologue
		   (let local-loop ()
		     (let-values
			 #,let-values-defs
		       (unless (and . #,(syntax-reverse loop-preconditions))
			       <exit-loop>)
		       (begin . #,body)
		       (begin . #,<increment-lists>)
		       (begin . #,iterations)
		       (cond ((and . #,loop-conditions)
			      (local-loop))
			     (else
			      <exit-loop>))))))))))
    ]

@CHUNK[<exit-loop>
       (begin
	 #,@epilogue
	 (#,return-continuation (or <generate-collection-type> count sum min max (void))))
       ]

The @scheme[return-continuation] is used for all exits from the loop. 

All the lists being iterated over are iterated just before all other iterations, including binding of loop variables
to the first element of the list. Each list has a corresponding variable that
is bound to the next element of the list via the @scheme[car] function. This binding is all that takes place during the
@scheme[iterations], and must happen after the lists themselves have been @scheme[cdr]'d off.

@CHUNK[<increment-lists>
       (let unroll-lists ((list-names (get-let-vars list-defs))
			  (result #'()))
	 (syntax-case list-names ()
	   (() result)
	   ((var . rest)
	    (unroll-lists #'rest #`((set! var (cdr var)) . #,result)))))
]

@section{Handling Conditional Statements}

Common Lisp's LOOP facility allows the use of @scheme[if] and @scheme[else] clauses that alter the behavior of the loop. Expansion of these
clauses proceeds as follows:

@itemlist[@item{ Rewrite all @scheme[when] clauses as @scheme[if] clauses }
	  @item{ Put the @scheme[if foo] clause into @scheme[current-condition], pushing any existing @scheme[current-condition] onto a stack first so that nested ifs can be handled. }
	  @item{ If an action clause (such as @scheme[do], @scheme[collect], @scheme[count], etc) is encountered while a
		     @scheme[current-condition] exists, combine the action clause and the @scheme[current-condition] into
		     a clause that can be added to a @scheme[cond] form (@scheme[current-cond-body])@italic{.} The @scheme[and] operator
		     is added to the front of the @scheme[current-condition] list, unless @scheme[current-condition] is the word @scheme[else]@italic{.} }
	  @item{ After an action clause, an @scheme[else] clause can be encountered, which goes into the @scheme[current-condition],
		  ultimately adding another clause to the @scheme[current-cond-body] when an action clause is encountered. }
	  @item{ If an @scheme[end] clause is encountered, a @scheme[cond] statement is created with the @scheme[current-cond-body]
		     and added to the @scheme[body]@italic{.} }
	  @item{ If an @scheme[if] clause is encountered after @scheme[if condition action-clause ...], rewrite it as if it was preceded
		     by @scheme[end]@italic{.} }
	  #:style 'ordered ]

@subsection{Managing a stack of nested if-clauses}

@racketblock[
    (loop for item in list
	  if (something? item)
	     if (something-else? item)
	        do (frobnicate item)
	     else if (third-thing? item)
	        do (replicate item)
	  else
	     (notify item))
    ]

The LOOP macro must support syntax like the above, with the effects you would expect based on the indentation shown. This
requires managing a stack. If an @scheme[if] clause is encountered while another one is already being processed, the
variables pertaining to the existing @scheme[if] must be pushed onto a stack, and popped from that stack when the end
of the nested @scheme[if] clause is reached. 

@CHUNK[<conditional-stack>
(define (push-cond)
  (set! conditional-stack
	(cons (list current-cond-body action-clauses current-condition) conditional-stack))
  (set-values! (current-cond-body action-clauses current-condition) (values #'() #'() #'())))

(define (pop-cond)
  (when (null? conditional-stack)
	(error "END without matching IF clause (may be implicit)"))
  (set-values! (current-cond-body action-clauses current-condition)
	       (apply values (car conditional-stack)))
  (set! conditional-stack (cdr conditional-stack)))
]

@subsection{Rewrite @scheme[when] and @scheme[unless] clauses}

The chunk of code below is evaluated within the context of a @scheme[syntax-case] macro called @scheme[loop-body].

@CHUNK[<rewrite-if-clauses>
       ((_ (when . rest))
	(parse-loop #`(loop-body (if . rest))))
       ((_ (unless condition . rest))
	(parse-loop #`(loop-body (if (not condition) . rest))))
       ((_ (else when . rest))
	(parse-loop #`(loop-body (else if . rest))))
       ((_ (else unless condition . rest))
	(parse-loop #`(loop-body (else if (not condition) . rest))))
]

@subsection{Collect @scheme[if] clauses}

@CHUNK[<collect-if-clauses>
       ((_ (if condition else . rest))
	(raise-syntax-error 'if-else "An action clause (such as do, collect, sum, etc) must occur between an if clause and an else clause"
			    #'(if condition else . rest)))
       ((_ (if condition . rest))
	(begin
	  (unless (syntax-null? current-condition)
		  (push-cond))
	  (set! current-condition #`(condition . #,current-condition))
	  (set! and? #t)
	  (parse-loop #'(loop-body rest))))
       ]

@subsection{Collect @scheme[else] and @scheme[else if] clauses}

@CHUNK[<collect-else-if>       
       ((_ (else if condition . rest))
	(begin
	  (set! current-cond-body
		(add-cond-clause current-condition action-clauses current-cond-body))
	  (set! current-condition #`(condition))
	  (set! and? #t)
	  (set! action-clauses #'())
	  (parse-loop #'(loop-body rest))))
       ((_ (else . rest))
	(begin
	  (when (syntax-null? current-condition)
		(raise-syntax-error 'else "else must be preceded by an if, when, or unless followed by action clauses\r\nconnected with 'and'. Example: if condition do condition and collect something" stx))
	  (set! current-cond-body
		(add-cond-clause current-condition action-clauses current-cond-body))
	  (set! current-condition #'else)
	  (set! and? #t)
	  (set! action-clauses #'())
	  (parse-loop #'(loop-body rest))))
]       

@subsection{AND}

If AND appears after an action clause, then a subsequent action clause will be part of the previous conditional.

@CHUNK[<and>
((_ (and . rest))
 (begin
   (set! and? #t)
   (parse-loop #'(loop-body rest))))
]


@subsection{END}

The @scheme[end] clause denotes the end of conditional processing. Action clauses after this will
be treated as unconditional, or as belonging to the outer-level if clause. Whatever @scheme[current-cond-body] is being
built gets inserted into the @scheme[body] at this point.

@CHUNK[<end>
	((_ (end . rest))
	 (begin
	   (when (and (syntax-null? current-condition)
		      (syntax-null? current-cond-body))
		 (raise-syntax-error 'end "end must be preceded by an if, when, or unless clause and an action clause." stx))
	   (set! and? #f)
	   (unless (syntax-null? current-condition)
		   (set! current-cond-body (add-cond-clause current-condition action-clauses current-cond-body))
		   (set! action-clauses #'())
		   (set! current-condition #'()))
	   (cond ((null? conditional-stack)
		  (set! body #`(#,@body (acond . #,(syntax-reverse current-cond-body))))
		  (set! current-cond-body #'()))
		 (else
		  (let ((cond-body #`(acond . #,(syntax-reverse current-cond-body))))
		    (pop-cond)
		    (set! action-clauses #`(#,@action-clauses #,cond-body)))))
	   (parse-loop #'(loop-body rest))))
]

@CHUNK[<add-cond-clause>
(define-for-syntax (add-cond-clause condition cond-body current-cond-body)
  #`((#,(fix-current-condition condition)
      . #,cond-body) . #,current-cond-body))

(define-for-syntax (fix-current-condition condition)
  (if (syntax-null? condition)
      #'()
      (syntax-case condition (else)
	((hd . tl) #`(and . #,(syntax-reverse condition)))
	(else condition))))
]

@section{Action clauses}

The action clauses are where the @scheme[current-condition] gets combined with some code to add to a
@scheme[current-cond-body].

Most action clauses only accept one form as an argument, but the @scheme[do] clause is special. Any compound form
(ie, one surrounded by parentheses) following the @scheme[do] clause is part of the action clause, and a @scheme[do] without
an action clause is illegal. This means the @scheme[do] form cannot be processed by rewriting it into another @scheme[do] form.
Instead, it is rewritten as a @scheme[do-internal] form, which is ignored if the form following it is not a compound form.

@CHUNK[<do-clause>
       ((_ (do (hd . tl) (hd2 . tl2) . rest))
	(parse-loop #'(loop-body (do-internal (hd . tl) and do (hd2 . tl2) . rest))))
       ((_ (do (hd . tl) . rest))
	(parse-loop #'(loop-body (do-internal (hd . tl) . rest))))
       ((_ (do non-list . rest))
	(raise-syntax-error 'do "Missing compound-form after do" #'(do non-list . rest)))
]

@subsection{Unavoidable boilerplate}

There are various chores that each action clause must accomplish on its own. Unfortunately, macro hygiene makes it
impossible to define macros to do it. The macros must be defined in a separate file due to limitations in Racket,
and due to the hygiene, local variables here wouldn't be visible to the code generated by the macro.

One thing that each action clause must do is detect whether two action clauses have been written in a row,
which breaks the conditional. For example in the following code:

@racketblock[ (loop for x from 1 to 10 if (even? x) collect x do (displayln x)) ]

..the @scheme[do] clause should be interpreted as if it was preceded by an @scheme[end] clause. That is, the @scheme[cond] form
that goes with the @scheme[if] clause should be generated before generating the code that implements the @scheme[do] clause.
The check for whether this should be done is:

@CHUNK[<action-boilerplate-guard>
       (or and?
	   (syntax-null? current-condition))
]

The @italic{current-condition} is the boolean expression (missing its @italic{and} operator) that determines whether the
action-clause should be executed. If there is no @italic{current-condition}, then that means the action-clause being
processed should be executed unconditionally. But if even if there @italic{is} a @italic{current-condition}, it may
be a stale @italic{current-condition} left over from a previous action-clause. The @italic{current-condition} is only
fresh if the @italic{and?} flag is set.

Therefore, if the above expression is true, it is safe to go ahead and process the action clause, otherwise an @scheme[end] clause must
be inserted and processed first. The code to do the inserting varies between action clauses just enough to prevent it from being
put in a chunk.

After the action-clause needs no further preprocessing, the next thing that must happen is that some code must be generated. What
exactly is generated differs between each action-clause, but all action-clause generated code goes into a list, where it is later
either placed into a @scheme[cond] form, or added naked to the @italic{body}. 

The generated snippet of code is added with this function:

@CHUNK[<define-add-action-clause>
       (define (add-action-clause clause)
	 (set! action-clauses #`(#,@action-clauses #,clause)))
]


After adding its action-clause code to the @italic{action-clauses} using @scheme[add-action-clause], every action-clause
must check if the clause is conditional or not. If the clause is conditional (that is, if @italic{current-condition}
										   is non-empty),
then the @italic{action-clauses} are left alone for later processing, and the @italic{body} is not modified, as
this will be handled in the @italic{end} clause. But
if the action-clause is unconditional, its contents must be added to the @italic{body} now.

@CHUNK[<action-boilerplate>
       (when (syntax-null? current-condition)
	     (set! body #`(#,@body #,@action-clauses))
	     (set! action-clauses #'()))
       (set! and? #f)
]


@CHUNK[<do-internal>
       ((_ (do-internal (hd . tl) . rest))
	(begin
	(cond (<action-boilerplate-guard>
	       (add-action-clause #'(hd . tl))
	       <action-boilerplate>
	       (parse-loop #'(loop-body (do-internal . rest))))
	      (else
	       (parse-loop #'(loop-body (end do-internal (hd . tl) . rest)))))))
]

When @scheme[do-internal] runs out of compound forms, then everything is placed into the @scheme[body] using
the boilerplate code.

@CHUNK[<do-internal/2>
       ((_ (do-internal . rest))
	(parse-loop #'(loop-body  rest)))
       ]

@subsection{Other Action Clauses}

@subsubsection{@scheme[return]}

In addition to being able to use the @scheme[return] macro in a @scheme[do] clause, the LOOP macro also supports
@scheme[return] as a clause. It can be simply implemented by rewriting it as a @scheme[do-internal] clause, the
only problem being that an extra compound-form following the @scheme[return] clause would be interpreted as
belonging to the @scheme[do-internal] clause. That case must be guarded against.

@CHUNK[<return-clause>
((_ (return value (hd . tl) . rest))
 (raise-syntax-error 'return "Extra value form after return" #'(return value (hd . tl))))
((_ (return value . rest))
 (parse-loop #'(loop-body (do-internal (return value) . rest))))
]

@subsubsection{@scheme[collect]}

The @scheme[collect] clause tells the loop to store a value in a collection list, which will
be returned.


@CHUNK[<collect-clause>
       <collect-into>
       ((_ (collect value . rest))
	(cond (<action-boilerplate-guard>
	       (set! initial-collection (syntax '()))
	       (add-action-clause #`(set! #,collection (cons value #,collection)))
	       <action-boilerplate>
	       (parse-loop #'(loop-body  rest)))
	      (else
	       (parse-loop #'(loop-body (end collect value . rest))))))
       ((_ (collecting value . rest))
	(parse-loop #'(loop-body (collect value . rest))))
]

The loop macro also supports collecting @italic{into} a specific variable.

@racketblock[ (loop for x in list when (odd? x) collect into odds) ]

Doing this requires a separate version of the above macro. It wasn't possible to combine the above's
functionality because there's no way to compare syntax-objects to tell if an identifier that appears
in the pattern equals @scheme[collection] or another macro variable.

Furthermore, because the collection variable can be accessed during iteration, and must be a list in the
correct order, it is not possible to cons the list in reverse order and then reverse it, as is done
with the implicit collector. Instead, adding to the end of the list is done with @scheme[append], which
makes @italic{collect into} O(n) for each iteration where a @italic{collect into} occurs. A loop that
uses @italic{collect into} on every iteration could be as slow as O(n@superscript{2}). Therefore, a warning
is issued every time @italic{collect into} is encountered at compile time.

Also, more than one @italic{collect into} clause can appear that refer to the same variable. Therefore, a
special procedure is needed that adds to the @scheme[let-defs] only if the variable is not already defined
there.

@CHUNK[<collect-into>
((_ (collect value into collector . rest))
 (cond (<action-boilerplate-guard>
	(displayln (format "***WARNING: ... collect ~a into ... has O(n) performance PER ITERATION. Your program will be EXTREMELY SLOW!" (syntax->datum #'value)))
	(displayln (format "      Use ... cons ~a into ... instead for O(1) performance." (syntax->datum #'value)))
	(add-action-clause #`(set! collector (append collector (list value))))
	<action-boilerplate>
	(maybe-add-let-def #'(collector '()))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end collect value into collector . rest))))))       
]

That function which adds the let-def only if it wasn't previously added, is defined as follows:

@CHUNK[<maybe-add-let-def>
(define (maybe-add-let-def definition)
   (define (already-defined? variable)
     (let loop ((let-defs let-defs))
       (syntax-case let-defs () 
	 (((var value) . rest)
	  (if (eq? (syntax->datum #'var)
		   (syntax->datum variable))
	      #t
	      (loop #'rest)))
	 (() #f))))
   (syntax-case definition ()
     ((var value)
      (unless (already-defined? #'var)
	  (set! let-defs #`((var value) . #,let-defs))))))
]       

@subsubsection{@scheme[cons]}

Because @italic{collect into} is such a uselessly pathological case in Racket (in contrast
with how useful it is in Common Lisp), an extension is provided: @scheme[cons into] operates like
@scheme[collect into], except the resulting list is seen in reverse order. There is no @scheme[cons]
without @scheme[into], and if there was, it'd be a synonym for @scheme[collect].

@CHUNK[<cons-into>
((_ (cons value into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(set! collector (cons value collector)))
	<action-boilerplate>
	(maybe-add-let-def #'(collector '()))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end cons value into collector . rest))))))
((_ (consing . rest))
 (parse-loop #'(loop-body (cons . rest))))
]

@subsubsection{@scheme[collect] variants}

The @scheme[collect] clause can return different types. The return type is controlled by
the @scheme[collection-type] variable, which can be set using the @scheme[with-collection-type]
clause. When the loop is about to return, it checks the collection-type and constructs
a return value as follows:

@CHUNK[<generate-collection-type>
       (begin
	 (if #,collection
	     (case #,collection-type
	       ((list) (if #,reverse? (reverse #,collection)
			   #,collection))
	       ((vector) (list->vector (reverse #,collection)))
	       ((string) (list->string (reverse #,collection)))
	       ((bytes) (list->bytes (reverse #,collection)))
	       ((hash) (list->hash #,collection))
	       ((hash/immutable) (list->hash/immutable #,collection)))
	     #f))
       ]

That last two conversions are not provided by Racket. They must be implemented here.

@CHUNK[<list->hash>
       (define-syntax define-list->hash
	 (syntax-rules ()
	   ((_ list->hash hash-return make pair set)
	    (define (list->hash lst)
	      (call/ec
	       (λ (return)
		  (loop with hash-return = (make)
			for pair in lst
			do set
			finally (return hash-return))))))))
       (define-list->hash list->hash hash-return make-hash pair (hash-set! hash-return (car pair) (cdr pair)))
       (define-list->hash list->hash/immutable hash-return make-immutable-hash pair (set! hash-return
											  (hash-set hash-return (car pair) (cdr pair))))
      ]

@CHUNK[<with-collection-type>
((_ (with-collection-type type . rest))
 (begin
   (case (syntax->datum #'type)
     ((list) #t)
     ((vector) #t)
     ((string) #t)
     ((bytes) #t)
     ((hash) #t)
     ((hash/immutable) #t)
     (else (raise-syntax-error 'with-collection-type "Unsupported collection type" #'type)))
   (set! collection-type #`'type)
   (parse-loop #'(loop-body rest))))
]

@subsubsection{@scheme[append]}

This is like @scheme[collect], except the value must be a list, which will be appended to the
end of the collection.

@CHUNK[<append-clause>
((_ (append value into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(set! collector (append collector value)))
	<action-boilerplate>
	(maybe-add-let-def #'(collector '()))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end append value into collector . rest))))))
((_ (append value . rest))
 (cond (<action-boilerplate-guard>
	(set! initial-collection (syntax '()))
	;; (add-action-clause #`(set! #,collection (reverse (append (reverse #,collection) value))))
	(add-action-clause #`(loop for item in value do
				   (set! #,collection
					 (cons item #,collection))))
	<action-boilerplate>
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end append value . rest))))))
((_ (appending . rest))
 (parse-loop #'(loop-body (append . rest))))
]

@subsubsection{@scheme[sum]}

This clause adds the given value to a numerical accumulator, which is then returned.

@CHUNK[<sum-clause>
((_ (sum value into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(set! collector (+ collector value)))
	<action-boilerplate>
	(maybe-add-let-def #'(collector 0))
	(set! let-defs #`((collector 0) . #,let-defs))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end sum value into collector . rest))))))
((_ (sum value . rest))
 (cond (<action-boilerplate-guard>
	(set! initial-sum #'0)
	(add-action-clause #`(set! #,sum* (+ #,sum* value)))
	<action-boilerplate>
	(parse-loop #'(loop-body rest)))
       (else
	(parse-loop #'(loop-body (end sum value . rest))))))
((_ (summing . rest))
 (parse-loop #'(loop-body sum . rest)))
]

@subsubsection{@scheme[count]}

Counts the number of times the expression evaluates as true.

@CHUNK[<count-clause>
((_ (count expression into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(when expression
				   (set! collector (add1 collector))))
	<action-boilerplate>
	(maybe-add-let-def #'(collector 0))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end count expression into collector . rest))))))
((_ (count expression . rest))
 (cond (<action-boilerplate-guard>
	(set! initial-count #'0)
	(add-action-clause #`(when expression
				   (set! #,count* (add1 count))))
	<action-boilerplate>
	(parse-loop #'(loop-body rest)))
       (else
	(parse-loop #'(loop-body end count expression . rest)))))
((_ (counting . rest))
 (parse-loop #'(loop-body (count . rest))))
]

@subsubsection{@scheme[minimize and maximize]}

This binds the smallest random number seen into @scheme[min-random]:

@racketblock[ (loop repeat 100 minimizing (random 100) into min-random ...) ]

@CHUNK[<min/max>
((_ (minimize expression into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(let ((temp expression))
			       (when (or (not collector)
					 (< temp collector))
				     (set! collector temp))))
	<action-boilerplate>
	(maybe-add-let-def #'(collector #f))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end minimize expression into collector . rest))))))
((_ (minimize expression . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(let ((temp expression))
			       (when (or (not min)
					 (< temp min))
				     (set! min temp))))
	<action-boilerplate>
	(parse-loop #'(loop-body rest)))
       (else
	(parse-loop #'(loop-body end count expression . rest)))))
((_ (minimizing . rest))
 (parse-loop #'(loop-body (minimize . rest))))

((_ (maximize expression into collector . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(let ((temp expression))
			       (when (or (not collector)
					 (> temp collector))
				     (set! collector temp))))
	<action-boilerplate>
	(maybe-add-let-def #'(collector #f))
	(parse-loop #'(loop-body  rest)))
       (else
	(parse-loop #'(loop-body (end count expression into collector . rest))))))
((_ (maximize expression . rest))
 (cond (<action-boilerplate-guard>
	(add-action-clause #`(let ((temp expression))
			       (when (or (not max)
					 (> temp max))
				     (set! max temp))))
	<action-boilerplate>
	(parse-loop #'(loop-body rest)))
       (else
	(parse-loop #'(loop-body end count expression . rest)))))
((_ (maximizing . rest))
 (parse-loop #'(loop-body (maximize . rest))))
]


@section{While and Until}

@racketblock[ (loop while keep-going ...) ]

@racketblock[ (loop until stop ...) ]

@CHUNK[<while/until>
((_ (while condition . rest))
 (begin
   (set! loop-preconditions
	 #`(condition . #,loop-preconditions))
   (parse-loop #'(loop-body rest))))
((_ (until condition . rest))
 (parse-loop #'(loop-body (while (not condition) . rest))))
]

@section{Repeating a Set Number of Times}

@racketblock[ (loop repeat 15 collect 'ocd) ]

@racketresult[(ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd ocd) ]

@CHUNK[<repeat>
((_ (repeat n . rest))
 (parse-loop #'(loop-body (for x from 1 to n . rest))))
]

@section{WITH: Binding variables}

@italic{With} is used like this:

@racketblock[ (loop with x = value ...) ]

It binds @italic{x} to the given @italic{value} by wrapping everything in a @scheme[let*] form. There is a variant:

@racketblock[(loop with x = value and y = other-value)]

This variant wraps using a @scheme[let] form instead of @scheme[let*]. Of course the two variants can be mixed,
producing a gnarled nest of @scheme[let] and @scheme[let*] forms over the body of the loop.

Furthermore, @scheme[x] can be a pattern:

@racketblock[
  (loop with ((a b) (c d)) = '((1 2) (3 4)) ...)
]

...where the elements in the lists can be destructured according to the pattern
@scheme[((a b) (c d))] to arbitrary depth. An older version of this macro  used
Racket's old @scheme[mzlib/match] to bind the variables, but then this case
was discovered:

@racketblock[
   (loop with (x y) = '((1 2 3) (4 5 6)) ...)
]

In Common Lisp, that matches the first two elements in each list,
but not the third, since there isn't a third variable. Even Common
Lisp's @scheme[DESTRUCTURING-BIND] cannot be used to implement this.


Furthermore, the list of variables is also allowed to be @italic{longer} than the data, in which
case Common Lisp assigns the value @scheme[NIL] to the variables that don't have corresponding
data in the list.

The loop's permissiveness when it comes to lists being shorter than the patterns they're matched against is not
explicitly implemented in the Lisp version, rather it comes indirectly as a consequence of @scheme[(CAR NIL)] and
@scheme[(CDR NIL)] returning @scheme[NIL] instead of raising an error like their Scheme/Racket counterparts. 

Scheme and Racket have the additional complication of having two distinct values,
@scheme[()] and @scheme[#f], where in Common Lisp, @scheme[NIL] is both the empty list and boolean false.

To get similar behavior in Racket, custom versions of @scheme[car] and @scheme[cdr] have been implemented. They return @scheme[#f] if
called with either @scheme[()] or @scheme[#f] as an argument:

@CHUNK[<cl-car-cdr>
(define (cl-car list)
  (and list
       (not (null? list))
       (car list)))

(define (cl-cdr list)
  (and list
       (not (null? list))
       (cdr list)))
]

SBCL uses a custom version of @scheme[DESTRUCTURING-BIND] named @scheme[LOOP-DESTRUCTURING-BIND], which is re-implemented in Racket as
@scheme[destructuring-let]:

@CHUNK[<destructuring-let>
<cl-car-cdr>
(define-syntax destructuring-let
  (syntax-rules ()
    ((_ let-kw (((hd . tl) a-value) . more-defs) rlet-defs let-defs . body)
     (destructuring-let let-kw
      ((tl (cl-cdr a-value))
       (hd (cl-car a-value))
       . more-defs) rlet-defs let-defs . body))
    ((_ let-kw ((() a-value) . more-defs) rlet-defs let-defs . body)
     (destructuring-let let-kw more-defs rlet-defs let-defs . body))
    ((_ let-kw ((atom a-value) . more-defs) rlet-defs let-defs . body)
     (destructuring-let let-kw more-defs ((atom a-value) . rlet-defs) let-defs . body))
    ((_ let-kw () (let-hd . let-tl) let-defs . body)
     (destructuring-let let-kw () let-tl (let-hd . let-defs) . body))
    ((_ let-kw () () let-defs . body)
     (let-kw let-defs . body))))
]

It is meant to be used like this:

@racketblock[
  (destructuring-let let* ((pattern-or-variable value) ...) () () body ...)
]

The two empty subforms are used to first accumulate the let-defs in reverse order, then re-accumulate them
in the correct order so that they'll work in a @scheme[let*] form. They expand to the type of @scheme[let] form
specified by the @scheme[let-kw].

@CHUNK[<with>
((_ (with x = value and y = other-value . rest))
 (begin
   (set! current-gnarled-let-def
	 #`(#,@current-gnarled-let-def (y other-value)))
   (parse-loop #'(loop-body (with* x = value . rest)))))
((_ (with* x = value and y = other-value . rest))
 (parse-loop #'(loop-body (with x = value and y = other-value . rest))))
((_ (with* x = value . rest))
 (begin
   (set! gnarled-let-defs
	 #`(#,@gnarled-let-defs (#,@current-gnarled-let-def (x value))))
   (set! current-gnarled-let-def #'())
   (parse-loop #'(loop-body rest))))
((_ (with x = value . rest))
 (begin
   (set! gnarled-let-defs #`(#,@gnarled-let-defs * ((x value))))
   (parse-loop #'(loop-body rest))))
]

@section{Iterating over stuff}

The @scheme[for] keyword denotes all forms of iteration:

@racketblock[
   (loop for variable preposition some-kind-of-collection ...)
]

In traditional Common Lisp, the @scheme[preposition] determines the type of @scheme[some-kind-of-collection]:

@itemlist[
   @item{@italic{in} to iterate over lists}
   @item{@italic{across} to iterate over arrays}
]

In Common Lisp, strings and vectors are both arrays, and Common Lisp has no equivalent to @scheme[bytes].

Common Lisp also provides the @scheme[on] preposition, which iterates over lists, except that the
@scheme[variable] is set to the entire remaining portion of the list instead of just the next element
in the list.

In this version of the loop macro, @scheme[across] iterates over vectors, strings, and bytes, while
@scheme[in] iterates over lists and hash tables, and @scheme[over] iterates over generators.


This version of the macro also iterates over hash-tables and generators.

All the variants of the for-clause can be captured by this @scheme[syntax-case] pattern:

@CHUNK[<universal-for-clause>
((_ (for . rest))
 (unless (and (syntax-null? current-condition)
	      (syntax-null? action-clauses)
	      (syntax-null? current-cond-body))
	 (raise-syntax-error 'loop "\"for\" must precede all \"if\", \"when\", \"collect\", and \"do\" clauses" stx))
 (syntax-case* #'(for . rest) <loop-literals> stx-compare
   <individual-for-clauses>
   (not-a-for-clause
    (parse-loop #'(loop-body not-a-for-clause)))))
]

The variations are all processed in a local @scheme[syntax-case] form.

@subsection{for x in y by iterator: List Iteration}

@racketblock[
 (loop for x in y [by iterator] ...)
]

Iterating over lists is the most basic case. On every iteration of the loop, @scheme[y] is iterated with the @scheme[iterator]
procedure (the default is @scheme[cdr]), and then @scheme[x] is pattern-matched against the first element of @scheme[y], unless @scheme[y]
is empty, in which case the loop terminates. If more than one of this or any @scheme[for] clause is used, then parallel iteration
occurs. The loop terminates when the first of the lists being iterated over is empty.

The @scheme[list-defs] let-bindings can hold a binding for the list @scheme[y], which
automatically results in the list being @scheme[cdr]'d as the loop progresses, while
the iterator variable @scheme[x] is part of the @scheme[let-defs]. Finally, @scheme[iterations] receives code that will update @scheme[x]
with each loop iteration. As with the @scheme[with] keyword, @scheme[x] can be a pattern, and if it is, it will be matched with the same
permissive semantics.

While iterating with a pattern, it is useful to be able to have a version of @scheme[set!] that pattern-matches the next value to be iterated over. SBCL uses a macro
called @scheme[SB-LOOP::LOOP-REALLY-DESETQ], which pattern-matches in a mutative fashion. The same approach will be used here.
An example use of the resulting macro would be:

@racketblock[
    (deset! (x y) a-list)
]

...which would set @scheme[x] and @scheme[y] to the first two values of @scheme[a-list], or @scheme[#f] if it ran out of values in @scheme[a-list]. The
macro is simpler than @scheme[destructuring-let] only because there is no need to create a @scheme[let] form. Otherwise, it has exactly the same matching
semantics as @scheme[destructuring-let]:

@CHUNK[<deset!>
(define-syntax deset!
  (syntax-rules ()
    ((_ (hd . tl) a-value)
     (begin
       (deset! hd (cl-car a-value))
       (deset! tl (cl-cdr a-value))))
    ((_ () a-value)
     (void))
    ((_ atom a-value)
     (set! atom a-value))))
]

@CHUNK[<for-x-in-y>
((for x in y by next . rest)
 (let ((y-binding (datum->syntax stx (gensym)))
       (next-binding (datum->syntax stx (gensym))))
   (set! iterations #`((set! #,y-binding (#,next-binding #,y-binding)) . #,iterations))
   (let ((iterate/check
	  #`(let ((it-is-null (null? #,y-binding)))
	      (unless it-is-null
		      (deset! x (car #,y-binding)))
	      (not it-is-null))))
     (set! loop-preconditions #`(#,iterate/check . #,loop-preconditions)))
   (parse-loop #`(loop-body (with #,next-binding = next with #,y-binding = y
				  with x = (if (null? #,y-binding)
					       #f
					       (car #,y-binding)) . rest)))))
((for x in y . rest)
 (parse-loop #`(loop-body (for x in y by cdr . rest))))
]

@subsection{for x on y by iterator: List iteration with entire lists.}


@racketblock[
   (loop for x on a-list [by iterator] ...)
]

This causes the pattern @scheme[x] to be matched on the entire remaining portion of @scheme[a-list] instead of only the first element. If @scheme[by iterator] is
included, then each successive @scheme[a-list] will be produced by @scheme[(iterator a-list)]. If @scheme[by iterator] is
omitted, then by default @scheme[cdr] is used as the @scheme[iterator].

@CHUNK[<for-x-on-y>
((for x on y by iter . rest)
 (let ((y-binding (datum->syntax stx (gensym)))
       (iter-binding (datum->syntax stx (gensym))))
   (set! iterations #`((set! #,y-binding (#,iter-binding #,y-binding))
		       . #,iterations))
   (let ((iterate/check #`(let ((it-is-null (null? #,y-binding)))
			    (unless it-is-null
			       (deset! x #,y-binding))
			    (not it-is-null))))
     (set! loop-preconditions #`(#,iterate/check . #,loop-preconditions)))
   (parse-loop #`(loop-body (with #,y-binding = y and #,iter-binding = iter with x = #,y-binding . rest)))))
((for x on y . rest)
 (parse-loop #'(loop-body (for x on y by cdr . rest))))
]


@subsection{for x being the hash-keys in table: Hash iteration}

This binds @italic{var} to each of the keys in the @italic{hash-table}
in succession:

@racketblock[
  (loop for var being the hash-keys in hash-table ...)
]

You can bind the corresponding hash value to another variable like this:

@racketblock[
  (loop for var being the hash-keys in hash-table using (hash-value other-var) ...)
]

The reverse is also supported:

@racketblock[
  (loop for var being each hash-value in hash-table using (hash-key other-var) ...)
]

@italic{@bold{Note: The @scheme[using] clause is broken. Common Lisp's @scheme[using] works totally differently
		    from this, but I haven't figured out the correct usage. The above example would not work in Common Lisp.
		    Also, @scheme[using] has many features that are not implemented here. }}

And the following extension is supported:

@racketblock[
  (loop for (key val) being the hash-pairs in hash-table ...)
]

@scheme[each] and @scheme[the] are interchangeable, as are the singular/plural forms of hash-keys, etc.

Iterating over hash tables is more difficult. Racket provides no way to get the "next" key and value pair from
a hash and remove it. Instead, it provides full-iteration functions such as @scheme[hash-map] and @scheme[hash-for-each].

A hash table can be rewritten as a list using @scheme[hash->list], but that would be a bad thing to do if the hash
was big.

The @scheme[hash-for-each] function can be used to create a generator, however, and the loop macro can iterate over generators. A generator using
@scheme[hash-for-each] will return @scheme[(void)] when iteration completes, but the loop macro requires @scheme[end-of-generator], because
@scheme[(void)] is ambiguous. So the clause is rewritten as an iteration over a generator.

Since Racket's hash iteration functions always provide both key and value, it makes sense to implement the
hash-pairs extension first. The singular @italic{hash-pair} can be used also, but it is simply rewritten
as @italic{hash-pairs}.

@CHUNK[<for-hash>
<for-hash-keys>
<for-hash-values>
((for (key value) being the hash-pairs in hash . rest)
 (parse-loop #`(loop-body (for (key value) over (make-hash-generator hash) . rest))))
((for (key value) being the hash-pair in hash . rest)
 (parse-loop #'(loop-body (for (key value) being the hash-pairs in hash . rest))))
((for something being each . rest)
 (parse-loop #'(loop-body (for something being the . rest))))
]

The generator is defined like this:

@CHUNK[<make-hash-generator>
(define (make-hash-generator hash)
  (generator ()
	     (begin 
	       (hash-for-each hash
			      (λ (k v)
				 (yield k v)))
	       (yield end-of-generator end-of-generator))))
]

@subsubsection{for x being the hash-keys in hash using...}

All of the standard Common Lisp variants for iterating over a hash table
are implemented in terms of the variant above.

@CHUNK[<for-hash-keys>
((for key being the hash-keys in hash using (hash-value value) . rest)
 (parse-loop #'(loop-body (for (key value) being the hash-pairs in hash . rest))))
((for key being the hash-keys in hash . rest)
 (parse-loop #'(loop-body (for (key value) being the hash-pairs in hash . rest))))
((for k being the hash-key in  hash . rest)
 (parse-loop #'(loop-body for k being the hash-keys in hash . rest)))
]

@CHUNK[<for-hash-values>
((for val being the hash-values in hash using (hash-key key) . rest)
 (parse-loop #'(loop-body (for (key value) being the hash-pairs in hash . rest))))
((for val being the hash-values in hash . rest)
 (parse-loop #'(loop-body (for (key value) being the hash-pairs in hash . rest))))
((for val being the hash-value . rest)
 (parse-loop #'(loop-body (for val being the hash-values . rest))))
]

@subsection{for x over y: Generator iteration}

For generator iteration, multiple values from @scheme[(yield)] are supported. The loop terminates when
the first of these values (or the only value) is @scheme[end-of-generator], whose value is defined
in this file.

Example:

@racketblock[
             (require racket/generator)
	     (loop for value over (generator ()
					   (for-each (λ (value)
							(yield value))
						     '(a b c d e f g))
					   end-of-generator))
	     ]

@CHUNK[<for-x-over-y>
((for (x . rest-vars) over y . rest)
 (let ((y-binding (datum->syntax stx (gensym))))
   (set! let-values-defs #`(((x . rest-vars) (#,y-binding)) . #,let-values-defs))
   (set! let-defs #`((#,y-binding y) . #,let-defs))
   (let set-precondition-loop ((variables #'(x . rest-vars)))
     (syntax-case variables ()
       (() #t)
       ((x . rest-vars) (begin (set! loop-preconditions
				#`((not (end-of-generator?  x)) . #,loop-preconditions))
			  (set-precondition-loop #'rest-vars)))))
   (parse-loop #'(loop-body rest))))
((for x over y . rest)
 (parse-loop #'(loop-body (for (x) over y . rest))))
]
   
@subsection{for x across y: Vector, string, and byte iteration}

@CHUNK[<for-x-across-y>
((for x across y . rest)
 (let* ((y-binding (datum->syntax stx (gensym)))
	(yix (datum->syntax stx (gensym)))
	(loop-condition #`(< #,yix (alen #,y-binding))))
   (set! let-defs #`((x #f) (#,yix 0) (#,y-binding y) . #,let-defs))
   (let ((iterate/check #`(let ((keep-looping? #,loop-condition))
			    (when keep-looping?
			       (set! x (aref #,y-binding #,yix)))
			    keep-looping?)))
     (set! loop-preconditions #`(#,iterate/check . #,loop-preconditions)))
   (set! iterations
	 #`((set! #,yix (add1 #,yix)) . #,iterations))
   (parse-loop #`(loop-body (with #,y-binding = y and #,yix = 0
			    with x = (if (>= #,yix (alen #,y-binding))
					 #f
					 (aref #,y-binding #,yix)) . rest)))))
]

@subsection{for x = y: Iterating over numbers}


@CHUNK[<for-x=y>
<for-x=y-then>
((for x = y . rest)
 (begin
   (set! gnarled-let-defs #`(#,@gnarled-let-defs ((x #f))))
   (set! loop-preconditions #`((begin (deset! x y) #t) . #,loop-preconditions))
   (parse-loop #'(loop-body rest))))
]

@CHUNK[<for-x=y-then>
((for x = y then step-form . rest)
 (begin
   (set! let-defs #`((x #f) . #,let-defs))
   (set! prologue #`(#,@prologue (set! x y)))
   (set! iterations #`(#,@iterations (set! x step-form)))
   (parse-loop #'(loop-body rest))))
]

@CHUNK[<for-x-from-low>
<for-x-from-low-to-high>
((for x from low by step . rest)
 (let ((step-binding (datum->syntax stx (gensym))))
   (set! let-defs #`((#,step-binding step) . #,let-defs))
   (parse-loop #`(loop-body (for x = low then (+ x #,step-binding) . rest)))))
((for x from low . rest)
 (begin
   (parse-loop #'(loop-body (for x from low by 1 . rest)))))
((for x downfrom high by decr)
 (parse-loop #'(loop-body (for x from high by (- decr) . rest))))
((for x downfrom high . rest)
 (parse-loop #'(loop-body (for x from high by -1 . rest))))
]

@CHUNK[<for-x-from-low-to-high>
((for x from low to high by step . rest)
 (let ((high-binding (datum->syntax stx (gensym))))
   (set! let-defs #`((#,high-binding #f) . #,let-defs))
   (set! prologue #`(#,@prologue (set! #,high-binding high)))
   (set! loop-preconditions
	 #`((<= x #,high-binding) . #,loop-preconditions))
   (parse-loop #'(loop-body (for x from low by step . rest)))))
((for x from low to high . rest)
 (begin
   (parse-loop #'(loop-body (for x from low to high by 1 . rest)))))
((for x from low below high by step . rest)
 (let ((high-binding (datum->syntax stx (gensym))))
   (set! let-defs #`((#,high-binding #f) . #,let-defs))
   (set! prologue #`(#,@prologue (set! #,high-binding high)))
   (set! loop-preconditions
	 #`((< x #,high-binding) . #,loop-preconditions))
   (parse-loop #'(loop-body (for x from low by step . rest)))))
((for x from low below high . rest)
 (begin
   (parse-loop #'(loop-body (for x from low below high by 1 . rest)))))
((for x from low upto high . rest)
 (begin
   (parse-loop #'(loop-body (for x from low to high . rest)))))
((for x from high downto low by step . rest)
 (let ((low-binding (datum->syntax stx (gensym))))
   (set! let-defs #`((#,low-binding #f) . #,let-defs))
   (set! prologue #`(#,@prologue (set! #,low-binding low)))
   (set! loop-preconditions
	 #`((>= x #,low-binding) . #,loop-preconditions))
   (parse-loop #`(loop-body (for x = high then (- x step) . rest)))))
((for x from high downto low . rest)
 (begin
   (parse-loop #'(loop-body (for x from high downto low by 1 . rest)))))
((for x downfrom high to low . rest)
 (parse-loop #'(loop-body (for x from high downto low . rest))))
((for x from high above low by step)
 (let ((low-binding (datum->syntax stx (gensym))))
   (set! let-defs #`((#,low-binding #f) . #,let-defs))
   (set! prologue #`(#,@prologue (set! #,low-binding low)))
   (set! loop-preconditions
	 #`((> x #,low-binding) . #,loop-preconditions))
   (parse-loop #`(loop-body (for x = high then (- x step) . rest)))))
((for x from  high above low . rest)
 (begin
   (parse-loop #'(loop-body (for x from high above low by 1)))))
]
 

@CHUNK[<individual-for-clauses>
<for-hash>
<for-x-in-y>
<for-x-on-y>
<for-x-over-y>
<for-x-across-y>
<for-x=y>
<for-x-from-low>
]

@section{FINALLY}

The @italic{finally} clause executes at the end of iteration. 

@CHUNK[<finally>
((_ (finally form . rest))
 (begin
   (set! epilogue #`(#,@epilogue form))
   (parse-loop #'(loop-body rest))))
]

@section{INITIALLY}

The @italic{initially} clauses execute at the beginning of iteration,
just after all variables have been initialized.

@CHUNK[<initially-clause>
((_ (initially form . rest))
 (begin
   (set! initially-prologue #`(#,@initially-prologue form))
   (parse-loop #'(loop-body rest))))
]
    

@section{@scheme[always], @scheme[never], and @scheme[thereis]}

@CHUNK[<always/never/thereis>
((_ (thereis form . rest))
 (let ((success? (datum->syntax stx (gensym))))
   (set! let-defs #`((#,success? #f) . #,let-defs))
   (set! body #`((when form
		       (set! #,success? #t)
		       (return #t)) . #,body))
   (parse-loop #`(loop-body (finally (return #,success?) . rest)))))
((_ (always form . rest))
 (begin
   (let ((success? (datum->syntax stx (gensym))))
     (set! let-defs #`((#,success? #t) . #,let-defs))
     (set! body #`((when (not form)
			 (set! #,success? #f)
			 (return #f)) . #,body))
     (parse-loop #`(loop-body (finally (return #,success?) . rest))))))
((_ (never form . rest))
 (parse-loop #'(loop-body (always (not form) . rest))))
]

@section{Fixing the Mistake That the R6RS Committee Made}

Racket loosely follows R6RS, which states that syntax literals, such as the ample number used in the implementation of this macro,
must refer to bindings, which can be overridden.  They did this with full awareness that doing this makes it possible to break
the basic syntax of the language. For example (and this example is used by the R6RS committee to specify what Scheme @italic{should} do),
the definition of @scheme[else] below breaks the @scheme[cond] form that follows it:

@interaction[
  (define else #f)
  (cond (#f 'not-this)
	(else 'should-return-this))
]

The LOOP macro has a lot of literal keywords, and I've added a few of my own.  One of these, @scheme[count], is already overridden by
Racket's library, but not by Scribble/LP, resulting in @scheme[count] not being able to be recognized as a @scheme[loop] keyword from
Racket. This program would produce a syntax error:

@racketblock[
    (loop count #t do (return))
]

Furthermore, it would be easy for someone to attempt to use this library along with another library that binds words like @scheme[from]
or @scheme[with] to something, and then they wouldn't be recognizable as keywords when used in this macro. That would be very
undesireable.

Also undesireable would be the result of following the advice given to me by Racket's developers. They suggested that I bind every single
one of these keywords:

@CHUNK[<loop-literals>
(for by as being by the each hash-key hash-keys hash-value hash-values hash-pair hash-pairs from while do do-internal collect collecting repeat repeating with with* sum summing append then
     appending matching nconc nconcing cons consing count counting string-append
     minimize minimizing maximize maximizing below above to downto downfrom upto in into on across over = until always never thereis and
     end else named initially finally if when unless return with-collection-type)
]

to a value or macro and then export them. The macro would still break if you required a library that has its own bindings to those words, if
you were even able to require both libraries at all and still have a program that compiles.

Fortunately, it seems that Racket's devs have run into this problem before, and I stumbled onto the @scheme[syntax-case*] form, which
allows you to specify your own procedure to compare symbols for the purpose of pattern matching. The procedure I created for this
considers two symbols to be equal if they @italic{look} equal to the naked eye:

@CHUNK[<stx-compare>
(define-for-syntax (stx-compare stx-1 stx-2)
  (eq? (syntax->datum stx-1)
       (syntax->datum stx-2)))
]


@CHUNK[<all-the-rest>
(define-syntax loop-body
  (λ (stx)
     <expansion-variables>
     <local-expander-functions>
     
     (let parse-loop ((stx stx))
       (define first-word (syntax-case stx ()
			    ((_ ()) #f)
			    ((_ (first . rest))
			       (syntax->datum #'first))))
       (syntax-case* stx <loop-literals> stx-compare
		     ((_ ())
		      (cond (<action-boilerplate-guard>
			     (let ((let-vars (get-let-vars let-defs)))
			       <loop-body>))
			    (else
			     (parse-loop #'(loop-body (end))))))
		     <with>
		     <initially-clause>
		     <finally>
		     <always/never/thereis>
		     <rewrite-if-clauses>
		     <collect-if-clauses>
		     <collect-else-if>
		     <while/until>
		     <repeat>
		     <and>
		     <end>
		     <do-clause>
		     <do-internal>
		     <do-internal/2>
		     <return-clause>
		     <collect-clause>
		     <min/max>
		     <cons-into>
		     <with-collection-type>
		     <append-clause>
		     <sum-clause>
		     <count-clause>
		     <universal-for-clause>))))
;(provide loop-body)
<outer-loop-macro>
]

@CHUNK[<local-expander-functions>
<define-add-action-clause>
<conditional-stack>
<maybe-add-let-def>
]

@section{The Outer Loop Macro}

The outer loop macro is the macro that is directly used by the user. It expands to either
the inner loop macro, called @scheme[loop-body], or to an optimized form. For example,
if the programmer writes this:

@racketblock[
    (loop for item in a-list collect (do-something-to item))
]

instead of expanding to the loop body seen above, it simply expands to this:

@racketblock[
(call/ec (λ (ec)
	    (parameterize ((return-cc ec))
			  (map (λ (item)
				  (do-something-to item)) a-list))))
]

The @scheme[call/ec] is necessary because the macro cannot prove that you're not doing this:

@racketblock[
  (loop for item in a-list collect (if (good? item)
				       item
				       (return 'bad-item-found!)))
]

...and if you @italic{are} doing that, the @scheme[call/ec] is required for it to work. The @scheme[for]
optimization is used for any number of @scheme[for] clauses as long as there is only one @scheme[collect] clause
and it occurs at the end. The additional @scheme[for] clauses result in more list arguments being passed to @scheme[map],
and more arguments being accepted by the lambda. Special care must be taken not to match against the destructuring
version of the for-loop, @scheme[(loop for (x (y z)) in a-list collect something)], since that requires special treatment
to get the destructuring part to work.

@CHUNK[<outer-loop-macro>
(begin-for-syntax
 (define (all-for-x-in-y/collect? stx)
   (cond ((syntax-null? stx)
	  #f)
	 (else
	  (let local-loop ((clauses stx))
	    (syntax-case* clauses (for in collect) stx-compare
	       ((for (hd . tl) in y . rest) #f)
	       ((for x in y collect z) #t)
	       ((for x in y . rest)
		(local-loop #'rest))
	       (_ #f))))))
 
 (define (expand-only-for-x-in-y/collect stx)
   (let local-loop ((clauses stx)
		    (lambda-args #'())
		    (lists #'()))
     (syntax-case* clauses (for in collect) stx-compare
	  ((for x in y collect z)
	   #`(call/ec
	      (λ (ec)
		 (parameterize ((return-cc ec))
			       (map (λ (x . #,lambda-args) z) . (y . #,lists))))))
	  ((for x in y . rest)
	   (local-loop #'rest #`(x . #,lambda-args) #`(y . #,lists)))))))

(define-syntax loop
  (λ (stx)
     (syntax-case* stx () stx-compare
	((_ . body)
	 (cond ((all-for-x-in-y/collect? #'body)
		(expand-only-for-x-in-y/collect #'body))
	   (else
	    (syntax-case* stx <loop-literals> stx-compare
		   ((_ named block-name . body)
		    #'(call/ec
		       (λ (block-name)
			  (loop . body))))
		   ((_ (hd . tl) . rest)
		    #'(loop do (hd . tl) . rest))
		   ((_ . body)
		    #'(loop-body body)))))))))
(provide loop)

]

@CHUNK[<local-macros>
<destructuring-let>
<deset!>
(define-syntax unwind-protect
  (syntax-rules ()
    ((_ value-form cleanup-form)
     (dynamic-wind (let ((ok? #t))
		     (lambda ()
		       (if ok?
			   (set! ok? #f)
			   (error "Re-entering UNWIND-PROTECT is prohibited."))))
	 (lambda () value-form)
	 (lambda () cleanup-form)))))

(define-syntax gnarled-let-nest
  (syntax-rules (*)
    ((_ () . body)
     (begin . body))
    ((_ (* (bindings ...) . more-bindings) . body)
     (destructuring-let let* (bindings ...) () ()
	(gnarled-let-nest more-bindings . body)))
    ((_ ((bindings ...) . more-bindings) . body)
     (destructuring-let let (bindings ...) () ()
       (gnarled-let-nest more-bindings . body)))
    ((_ (* (bindings ...)) . body)
     (destructuring-let let* (bindings ...) () () . body))
    ((_ ((bindings ...)) . body)
     (destructuring-let let (bindings ...) () () . body))))
]

@CHUNK[<supporting-functions>
(define-for-syntax (syntax-null? stx)
  (syntax-case stx ()
    (() #t)
    (_ #f)))

(define-for-syntax (syntax-reverse stx)
  (let loop ((rest stx)
	     (result #'()))
    (syntax-case rest ()
      (() result)
      ((hd . tl) (loop #'tl #`(hd . #,result))))))

(define-for-syntax (get-let-vars stx)
  (let loop ((rest stx)
	     (result #'()))
    (syntax-case rest ()
      (() (syntax-reverse result))
      (((var . value) . rest)
       (loop #'rest
	     #`(var . #,result))))))

(define-for-syntax (syntax-find pred? syntax-list)
  (let loop ((rest syntax-list))
    (syntax-case rest ()
      (() #f)
      ((hd . tl)
       (if (pred? #'hd)
	   #'hd
	   (loop #'tl))))))

(define-for-syntax (add-iterations let-vars iterations)
  (let loop ((let-vars let-vars)
	     (iterations iterations)
	     (result #'()))
    (syntax-case let-vars ()
      (() (syntax-reverse result))
      ((var . rest)
       (let ((iter (syntax-find (λ (stx)
				   (syntax-case stx ()
				       ((var2 . fuckit)
					(eq? (syntax->datum #'var2)
					     (syntax->datum #'var)))))
				iterations)))
	 (loop #'rest iterations (if iter (syntax-case iter ()
					    ((var body) #`(body . #,result)))
				     #`(var . #,result))))))))
			       

(define (aref arr n)
  ((cond ((vector? arr) vector-ref)
	 ((string? arr) string-ref)
	 ((bytes? arr) bytes-ref)) arr n))

(define (alen arr)
  ((cond ((vector? arr) vector-length)
	 ((string? arr) string-length)
	 ((bytes? arr) bytes-length)) arr))

<stx-compare>
   
(define-for-syntax (print-syntax stx)
  (displayln (syntax->datum stx)))
<list->hash>

]
