#!r6rs

(library 
 (shred)
 (export epsilon-condition epsilon?
         list-head seq->list make-list-iterator
         make-simple-rule make-sequence-matcher
         &invalid-rule-violation &invalid-seq-violation)
 (import 
  (rnrs (6))
  (rnrs base (6))
  (rnrs enums (6))
  (rnrs hashtables (6))
  (rnrs lists (6))
  (rnrs exceptions (6))
  (rnrs conditions (6))
  (rnrs records syntactic (6))
  (srfi :14))
 
 (define epsilon-condition 'EPSILON)
 
 (define zero?
   (lambda (x)
     (eq? 0 x)))
 
 (define epsilon?
   (lambda (seq)
     (eq? seq epsilon-condition)))
 
 (define-condition-type &invalid-rule-violation
   &lexical
   make-invalid-rule-violation invalid-rule-violation?)
 
 (define-condition-type &invalid-message-violation
   &lexical 
   make-invalid-message-violation invalid-message-violation?)
 
 (define arity-exception
   (lambda (closure-type method arity actual)
     (raise
      (condition
       (make-who-condition object-type)
       (make-message-condition (string-append
                                (string-append "Invalid number of arguments for method " method)
                                (string-append
                                 (string-append " in a closure of type " closure-type)
                                 (string-append
                                  (string-append ": expected " arity)
                                  (sringt-append "received " actual))))) 
       (make-invalid-message-violation)))))
 



(define make-tester
  (lambda (test rules)
    (lambda (item)
      (test rules item))))

(define-record-type (match make-match match?)
  (fields
   (result get-result)
   (continuation-list get-continuation-list)))

(define make-simple-matcher
  "Utility function for generating a simple matcher 
    for an atomic item in a sequence from a rule with
    a single match condition and a single continuation."
  (lambda (test continuation)
    (lambda (item)
      (if (null? item)
          '()
          (make-match (test item) (list continuation))))))

(define make-sequence-matcher
  "Utility function for applying a rule to a repeated sequence 
    of items (e.g., a Kleene star sub-expression in a regex)."
  (lambda (rule)
    (lambda (seq)
      (let ((match (rule seq)))
        (if (null? match)
            '()
            (list match
                  (let self ((new-seq seq))
                    (if (zero? (new-seq 'balance))
                        epsilon-condition
                        (