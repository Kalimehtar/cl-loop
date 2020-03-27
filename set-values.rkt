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


(provide set-values! destructuring-bind)
