;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
(defun BFS (FRINGE)
	(cond ((not (car FRINGE)) nil)						;check for end of some list
	      ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))	;append leaves to the fringe, and keep processing
	      (t  (BFS (append (cdr FRINGE) (car FRINGE))))			;if a subtree is encountered, bring all the nodes there up to the current level
	)
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S (list T T T T))  ; check for equality with goal state
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

(defun NEXT-STATE (S A)
	(let* ( (h (first S)) (b (second S)) (d (third S)) (p (fourth S)) 			; convenience bindings for reference
		(is_everyone_on_the_same_side (or (and b d p) (not (or b d p)))) 		; to check for all nil or all t
		;(is_leaving_b_w_p_or_d (and (and h b) (or d p)))
		(is_leaving_b_w_p_or_d (or (or (and b d) (not (or b d))) (or (and b p) (not (or b p)))))	;check if leaving baby with poison or dog
		(is_not_with_b (and (or b h) (not (and b h))))							;check if homer is unable to move something / not on the same side
		(is_not_with_d (and (or d h) (not (and d h))))
		(is_not_with_p (and (or p h) (not (and p h))))
	      )
	      ( cond ((equal A 'h) (cond (is_leaving_b_w_p_or_d nil) (t (list (not h) b d p))))			;using prior checks, return nil for invalid moves, and the new state otherwise
		      ((equal A 'b) (cond (is_not_with_b nil) (t (list (not h) (not b) d p))))
		      ((equal A 'd) (cond (is_everyone_on_the_same_side nil) (is_not_with_d nil) (t (list (not h) b (not d) p))))
		      ((equal A 'p) (cond (is_everyone_on_the_same_side nil) (is_not_with_p nil) (t (list (not h) b d (not p)))))
              )
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
	(let* (
		;(debug_succ (print S))

		(homer_attempt (NEXT-STATE S 'h))						;attempt all four possible action
		(baby_attempt (NEXT-STATE S 'b))
		(dog_attempt (NEXT-STATE S 'd))
		(poison_attempt (NEXT-STATE S 'p))

		(with_h (cond (homer_attempt (list homer_attempt)) (t '())))			;filter invalid/nil result states from attempted moves
		(with_b (cond (baby_attempt (cons baby_attempt with_h)) (t with_h)))
		(with_d (cond (dog_attempt (cons dog_attempt with_b)) (t with_b)))
		(with_p (cond (poison_attempt (cons poison_attempt with_d)) (t with_d)))
	      )
	      
	      with_p										;return filtered result 
	)
    
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond ((not STATES) nil)		;check for end of path
	      ((equal S (car STATES)) t)	;check for equality to some path
	      (t (ON-PATH S (cdr STATES)))	;iterate down path if n match
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	(let (
		(next_state (car STATES))
	     )
	     (cond ((not next_state) nil)						;no legal states left - return nil
		   ;((equal next_state '(t t t t)) (append PATH (list next_state)))	;goal state reached - return path - edited out to leave to dfs
		   (t (let (								;perform DFS with the current successor and path
				(result (DFS next_state PATH))				;call to dfs
			   )
			   (cond ((not result) (MULT-DFS (cdr STATES) PATH))		;branch did not contain goal state - try next successor
			         (t result)						;branch contained goal state
			   )
		      )
		   )
	     )
	)
	
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
	(let (
		(new_path (append PATH (list S)))					;the path with the current state appended, for reference
		;(debug_dfs (print S))							;debug line
	     )
	     (cond ((FINAL-STATE S) new_path)						;check for final state
		   (t (let* (
				;(debug_rec (print 3))

				(raw_successors (SUCC-FN S))				;get valid successor (without checking for cycles)
				(succ1 		(first raw_successors))			;get possible succesors for reference
				(succ2		(second raw_successors))
				(succ3		(third raw_successors))

				(with1 (cond ((ON-PATH succ1 new_path) '()) (t (list succ1))))	;filter out repeated successor states
				(with2 (cond ((not succ2) with1) ((ON-PATH succ2 new_path) with1) (t (cons succ2 with1))))
				(with3 (cond ((not succ3) with2) ((ON-PATH succ3 new_path) with2) (t (cons succ3 with2))))

				;(debug2 (print with3))					;debug
			    )
		    	    (MULT-DFS with3 new_path)
		     )
		   )
	     )
	)
)
    
