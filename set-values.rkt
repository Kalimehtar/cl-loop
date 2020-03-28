#lang racket

;; These macros had to be defined in this separate file because of a
;; bug in Racket.

(require mzlib/match)

(define-syntax destructuring-bind
  (syntax-rules ()
    ((_ formals structured-value . body)
     (match structured-value
		 (formals . body)))))


(define-syntax set-values-list!
  (syntax-rules ()
    ((_ (var) list) (set! var (car list)))
    ((_ (var . rest) list) (let ((rest-values (cdr list)))
			     (set! var (car list))
			     (set-values-list! rest rest-values)))))
(define-syntax set-values!
  (syntax-rules ()
    ((_ variables value-form)
     (let ((value-list (call-with-values (λ () value-form) list)))
       (set-values-list! variables value-list)))))

(define-for-syntax (find-it stx)
  (syntax-case stx ()
    (() #f)
    (((hd ...) tl ...) (find-it #'(hd ... tl ...)))
    ((hd . tl) (if (eq? (syntax-e #'hd) 'it) #'hd (find-it #'tl)))))

(define-syntax (acond stx)
  (syntax-case stx (else)
    [(acond [else . else-body])
     #'(begin . else-body)]
    [(acond)
     #'(void)]
    [(acond [condition body ...] rest ...)
     (with-syntax ((it (datum->syntax (or (find-it #'(body ...)) stx) 'it)))
       #'(let ([it condition])
           (if it
               (begin body ...)
               (acond rest ...))))]))



(provide set-values! destructuring-bind acond)
