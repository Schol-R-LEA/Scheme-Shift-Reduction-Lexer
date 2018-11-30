#!r6rs

(library 
 (shred fsa)
 (export make-finite-state-automata)
 (import 
  (rnrs (6))
  (rnrs base (6)))

 (define make-finite-state-automata
   (lambda (state-transitions)
     (let ((state 0)
	   (table (list->vector state-transitions)))
       (lambda (input)
	 (let* ((previous state)
		(transitions (vector-ref state table))
		(lookup-fn (vector-ref 0 transitions))
		(next (lookup-fn input transitions)))
	   (if (error? next)
	       (raise fsa-invalid-transition-error)
	       (begin
		 (set! state next)
		 (list previous next))))))))
