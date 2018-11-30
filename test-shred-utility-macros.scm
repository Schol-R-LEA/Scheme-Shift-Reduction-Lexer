#!r6rs
(import 
 (rnrs (6))
 (rnrs base (6))
 (rnrs io simple (6))
 (srfi :64)
 (shred))

(define runner (test-runner-simple))

(test-with-runner runner 
  (test-group
   "Tests of the state definition template macro and support functions"
   (test-group
    "Basic matcher generation - a single atomic rule"
    (let ((matcher (
                                                
					 
