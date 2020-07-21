; Code for Homework 1 
; CS161: Artificial Intelligence, Fall 2019, UCLA
; Written by Pravin Visakan UID:504825910

; Overall Explanation of My Solutions

; For Question 1, I took the already recursive definition of Padovan numbers from the spec, and rendered it in Lisp using the cond statement (see function comment for details). Because the spec defines the function in terms of n+1, I had to offset the recursive step by one.

; For Question 2, I followed the same basic recursive structure as in 1, but simply added one whenever a recursive step was taken. Since the base cases, by definition, don't require any addition, these contribute 0 to the result, and terminate the recursion.

; For Question 3, I used the car and cdr operators to decompose the given list. Since function call was designed to handle list inputs, elements of the initial list that were lists themselves (i.e. branches) get handled by the same logic. Using the atom operator, we can detect and replace the base case, tree leaves. The nil case prevents the end of the list from adding an extra '?, as (atom nil) returns true.


; Test helper function
; Runs the provided function object func with integers N-0 as a single argument, and returns a list of the results
; NOTE: this uses the unapproved funcall keyword, but this is only to facilitate testing!
(defun test (func N)
	(cond ((= N 0) (cons (funcall func 0) nil))
	      (t (cons (funcall func N ) (test func (- N 1)) ))
	)
)

; Question 1
; calculates the Nth Padovan number, defined numerically as PAD(n+1) = PAD(n-1) + PAD(n-2), with n=0,1, or 2 resulting in 1. 
(defun PAD (N)
	(cond ((= N 0) 1)				; base cases
	      ((= N 1) 1)				; note: will loop infintely on negative num
	      ((= N 2) 1)				; ">" is not allowed per the spec
	      (t (+ (PAD (- N 2)) (PAD (- N 3))))) 	; recursive step 
							; note: though the operands differ from the
							; 	definition, the math checks out
)
; test cases
(print "PAD test:")
(print (test #'PAD 10))

; Question 2
; Returns the number of additions needed to calculate the Nth Padovan number, as defined above.
(defun SUMS (N)
	(cond ((= N 0) 0)				; base cases
	      ((= N 1) 0)				; note: no addition is needed in this case
	      ((= N 2) 0)				
	      (t (+ 1 (SUMS (- N 2)) (SUMS (- N 3))))) 	; recursive step 
)
; test cases
(print "SUMS test:")
(print (test #'SUMS 10))

; Question 3
; Takes a tree defined as a nested list structure, and anonymizes it, i.e. replace every atom/leaf value with a '?
(defun ANON (node)
	(cond ((not node) nil)				; to handle the end of a list
	      ((atom node) '?)				; anonymize a leaf
	      (t (cons (ANON (car node)) (ANON (cdr node)))))	; for intermediary nodes, process both elements
)

; test cases
(print (ANON '42))
(print (ANON 'FOO))
(print (ANON '(((L E) F) T)))
(print (ANON '(5 FOO 3.1 -0.2)))
(print (ANON '(1 (FOO 3.1) -0.2)))
(print (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
(print (ANON '(R (I (G (H T))))))
