#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                  (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                  (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
  (let ([number  framenumber ]
        [bindings hashtable])
    (begin
      (set! framenumber (+ framenumber 1))
      (frame number bindings parent))))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
  (match prog
    [(pgm deflist) (let ((global-frame (findf (lambda (x) (equal? (emptyframe) (frame-parent x))) stack)))
                     (begin
                       (for-each
                        (lambda (x) (processdef x global-frame))
                        deflist)
                       (return-value-of-main global-frame)))]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set!
                    (frame-bindings fr)
                    v/f
                    (eval-exp exp))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (hash-ref
                        (frame-bindings (search exp (car stack)))
                        exp)]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var _) (closure exp
                                      (car stack))]
                [(app exp1 explist) (let* ((clos (eval-exp exp1))
                                           (clos-lam (closure-lambda clos))
                                           (clos-frame (closure-frame clos))
                                           (deflist (map (lambda (x y) (def x y)) (lam-varlist clos-lam) explist))
                                           (new-frame (createframe (make-hash '()) clos-frame)))
                                      (begin
                                        (for-each (lambda (x) (processdef x new-frame)) deflist)
                                        (push new-frame)
                                        (let ((value (eval-exp (lam-exp clos-lam))))
                                          (begin (pop)
                                                 value))))]
                [(iff cond exp1 exp2) (if (eval-exp cond)
                                          (eval-exp exp1)
                                          (eval-exp exp2))]
                [(sett var exp) (hash-update!
                                 (frame-bindings (search var (car stack)))
                                 var
                                 (lambda (x) (eval-exp exp)))]
                [(lett deflist exp) (begin
                                      (let ((new-frame (createframe (make-hash '()) (car stack))))
                                        (begin
                                          (for-each
                                           (lambda (x) (processdef x new-frame))
                                           deflist)
                                          (push new-frame)))
                                      (let ((e (eval-exp exp)))
                                        (begin
                                          (pop)
                                          e)))]
                [(lets deflist exp) (process-lets deflist exp)]
                [(beginexp explist) (process-beginexp explist)]
                [(defexp deflist exp) (begin
                                        (for-each
                                         (lambda (x) (processdef x (car stack)))
                                         deflist)
                                        (eval-exp exp))]
                [(debugexp) (begin
                              (vector-set! stacks stacksindex stack)
                              (set! stacksindex (+ 1 stacksindex)))])]))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
    [(cons def1 '()) (eval-exp def1)]
    [else  (begin
             (eval-exp (car explist))
             (process-beginexp (cdr explist)))]))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    ['() (eval-exp exp)]
    [else (eval-exp
           (lett
            (list (car deflist))
            (lets (cdr deflist) exp)))]
    ))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (cond [(equal? (frame-parent fr) (emptyframe)) (displayln fr)
                                                 (displayln "@@@@@@@@@@@@@@@@@@@@@@@")]
        [else (begin
                (displayln fr)
                (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
                (print-current-environment (frame-parent fr)))])
  )

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
  
  (cond [(equal? fr (emptyframe)) (displayln sym)
                                  (error "Symbol not found")]
        [else (let ((found? (hash-ref
                             (frame-bindings fr)
                             sym
                             (lambda () #f))))
                (if found?
                    fr
                    (search sym (frame-parent fr))))]))


(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))

               


