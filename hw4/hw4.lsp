;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
	(+ (* (- n 1) k) c)
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
	(if (<= c k)
		(append (list (node2var n c k)) (at-least-one-color n (+ c 1) k))
		nil
	)
  )


; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;

;helper - returns a list of clauses with c1, c2 for a given n, for all c2 where c1< c2 <= k
(defun pairs_ahead (n c1 c2 k) 
	(if (<= c2 k)
		(cons (list (- (node2var n c1 k)) (- (node2var n c2 k))) (pairs_ahead n c1 (+ c2 1) k))
		nil
	)
)
; call the helper for every color 1-k, thereby getting every disjunct pair
(defun at-most-one-color (n c k)
	(if (<= c k)
		(append (pairs_ahead n c (+ c 1) k) (at-most-one-color n (+ c 1) k))
		nil
	)
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; form a conjunction of at-least-one and at-most-one colors, for 1-color
(defun generate-node-clauses (n k)
	(append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;

; helper function - get the same-color var pairs of node n1 and n2, invert them, form a disjunction; build a conjunction of these for c from 1 to k
(defun get_pairs (n1 n2 c k)
	(if (<= c k)
		(cons (list (- (node2var n1 c k)) (- (node2var n2 c k))) (get_pairs n1 n2 (+ c 1) k))
		nil
	)
)

; decompose the edge into its nodes, then clauses representing (~a ^ ~b), where a and b are c assigned to n1 and n2, for all c from 1-k
(defun generate-edge-clauses (e k)
	(get_pairs (car e) (cadr e) 1 k)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
