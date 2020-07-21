;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)

; goal-test-row - helper function, search for a box in a given row

(defun goal-test-row (row)
	(cond ((not row) t)			; base case - no boxes in this row
	      ((isBox (car row)) nil)		; base case - found a box, not a goal state
	      (t (goal-test-row (cdr row)))	; not a box - continue onto next square
	);end cond

);end defun

; goal-test - the goal test implementation for an a-star implementation
(defun goal-test ( s)
	(cond ((null s) t)					 ; base case - every square was checked, and none were boxes
	      ((goal-test-row (car s)) (goal-test (cdr s)))	 ; test row - if no boxes are found, move on to next row
	      (t nil)						 ; a box was found - return nil
	);end cond
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; get-square - helper function, returns the contents of s at r,c
(defun get-square (s r c )
	(if (and (> r -1) (> c -1) (< r (length s)) (< c (length (car s))))			; boundary check
		(nth c (nth r s))							; retrieve the sublist/row r, then the element of that row at c
		42
	)
);end defun

; set-square - helper function, returns a state equal to s with r,c set to v
(defun set-square (s r c v)
	(let* ( (row (nth r s))							; get current row
		(first_rows (butlast s (- (length s) r)))			; get rows before and after current
		(last_rows (nthcdr (+ r 1) s))
		(first_cols (butlast row (- (length row)  c)))			; get elements before and after current
		(last_cols (nthcdr (+ c 1) row))
		(new_row (append first_cols (list v) last_cols))		; append to form new row with v
	      )
	      (append first_rows (list new_row) last_rows)			; append rows to get new state
	);end let
);end defun

; try-move - helper function, tries to move keeper in some direction
; s - state, dir - direction, m - value to be moved (box or keeper) *-offset - position to offset m by *_new - new location to try moving to
(defun try-move (s m r_offset c_offset r_new c_new)
	(let* ((target (get-square s r_new c_new))									; get whatever is at the target location
	      (original (get-square s (- r_new r_offset) (- c_new c_offset)))						; get whatevers at original square
	      (blank_state (set-square s (- r_new r_offset) (- c_new c_offset) (if (equal m original) blank star))) 	; get the current state with the moved object removed
	     )
	     (cond ;((or (< r_new 0) (< c_new 0)) nil) 									; boundary checking - top
		   ;((or (> r_new (length (cdr s))) (> c_new (length (cadr s)))) nil)					; boundary check bottom (- 1 via length of cdrs)
		   ((equal target 42) nil)
		   ((isBlank target) (set-square blank_state r_new c_new m))						; blank - set the new square to m
		   ((isWall target) nil)
		   ((isBox target) 
			(if (isBox m) 
				nil											; moving a box to another box is nil 
				(let ((box_move (try-move blank_state box r_offset c_offset (+ r_new r_offset) (+ c_new c_offset)))) ; move box if currently moving player
				     (if box_move										     ; if move was valid, continue
					(set-square box_move r_new c_new m)
					nil
				     );end if
				);end let
			);end if 
		   );end isBox case
		   ((isStar target) (set-square blank_state r_new c_new (+ m 3)))					; goal - set the new square to the corresponding goal+m square
		   ((isBoxStar target) 
			(if (isBox m) 
				nil											; moving a box to another box is nil 
				(let ((box_move (try-move blank_state box r_offset c_offset (+ r_new r_offset) (+ c_new c_offset)))) ; move box if currently moving player
				     (if box_move										     ; if move was valid, continue
					(set-square box_move r_new c_new (+ m 3)) ;set to goal variant of moving object
					nil
				     );end if
				);end let
			);end if 
		   );end isBoxStar case
	     ); end cond
	)
);end defun


; next-state - returns valid new states for the a-star algorithm
(defun next-states (s)
  (let* (;(debug_state (print s)) ;debug variable
	 (pos (getkeeperposition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 ; in order, we try - DOWN UP LEFT RIGHT
	 (result (list (try-move s keeper 1 0 (+ y 1) x) (try-move s keeper -1 0 (+ y -1) x) (try-move s keeper 0 -1 y (+ x -1)) (try-move s keeper 0 1 y (+ x 1))))
	 )
    (cleanUpList result);end
   );end let
);end defun

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;NOTE - this heuristic is admissable, because it's essentially a version of the game with the constraint of having to move to the boxes removed - it's as if the boxes can be moved arbitrarily
; 	A solution to the full game is still a solution to the relaxed game, so this works
(defun h1 (s)
	(cond ((null s) 0)				; done searching through s, return 0
	      (t (+ (count 2 (car s)) (h1 (cdr s)))) 	; count the number of boxes in this row, then continue
	)  
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; helper function - finds the position of the first box
(defun getFirstBoxColumn (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r))
	       col
	     (getFirstBoxColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)

; finds the position of the first box 
(defun getFirstBoxPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getFirstBoxColumn (car s) 0)))
	     (if x
		 ;box is in this row
		 (list x row)
		 ;otherwise move on
		 (getFirstBoxPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

; new attempt - get closest box position
; helper function - finds the position of the closest box
;(defun getClosestBoxColumn (r col)
;  (cond ((null r) nil)
;	(t (if (isBox (car r))
;	       col
;	     (getFirstBoxColumn (cdr r) (+ col 1))
;	     );end if
;	   );end t
;	);end cond
;)

; finds the position of the first box 
;(defun getClosestBoxPosition (s row col addFlag)
;  (cond ((null s) nil)
;	(t (let ((x (getClosestBoxColumn (car s) col)))
;	     (if x
;		 ;box is in this row
;		 (list x row)
;		 ;otherwise move on
;		 (getClosestBoxPosition (cdr s) (+ row 1))
;		 );end if
;	       );end let
;	 );end t
;	);end cond
;  );end defun

; isStar2 - adds support for for null inputs
(defun isStar2 (v)
  (equal v star)
)

; reverseList - reverse a list using appends, used for backwards traversal in following
(defun reverseList (s accum)
	(if (null s)
		accum						; check for end of list
		(reverseList (cdr s) (append (list (car s)) accum))	; tail-recursive call
	);end if
)

; helper function - finds the position of the first box
(defun getClosestGoalColumn (r_front r_back col)
  (cond ((and (null r_front) (null r_back)) nil)			; base case - both rows are searched
	((isStar2 (car r_front)) col)					; return col if found in forward
	((isStar2 (car r_back)) (- col))				; return -col if found in back
	(t (getClosestGoalColumn (cdr r_front) (cdr r_back) (+ col 1)))
  );end cond
)

; finds the position of the first box 
(defun getClosestGoalPosition (s_front s_back accum_row original_row)
  (cond ((and (null s_front) (null s_back)) nil) 		;base case - both states are empty
	(t (let ((x (getClosestGoalColumn (car s_front) (car s_back) 0)))
	     (if x
		 ;box is in this row
		 (list (abs x) (+ accum_row original_row))
		 ;otherwise move on
		 (getClosestGoalPosition (cdr s_front) (cdr s_back) (+ accum_row 1) original_row)
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

; finds the manhattan distance between the first box and the keeper, and then the manhattan distance from the box and its closest goal, and sums them
; admissable - addes
(defun h504825910 (s)
	;(max
		(let* ((pos (getkeeperposition s 0))			;get keeper position
			 (kx (car pos))
			 (ky (cadr pos))
			 (bpos (getFirstBoxPosition s 0))		;get position of first box
			 (bx (car bpos))
			 (by (cadr bpos))
			 (by2 (if by by 0))
			 (gpos (getClosestGoalPosition (nthcdr by2 s) (reverseList (butlast s by2) nil) 0 by))		;get position of closest gate 
			 (gx (car gpos))
			 (gy (cadr gpos)))
			 ;(gx bx)
			 ;(gy by))
			 
			(if (and kx ky bx by gx gy)					; check if an answer was found
			    (+ (abs (- kx bx)) (abs (- ky by)) (abs (- bx gx)) (abs (- by gy))) ; calculate a sum
			    0							
			);end if
		);end let

		;(cond ((null s) 0)				; done searching through s, return 0
		;      (t (+ (count 2 (car s)) (h1 (cdr s)))) 	; count the number of boxes in this row, then continue
		;);end cond
	;);end max  
  
)

; an attempt at a really quick heuristic to beat h1 - the above is too complex
(defun h5new (s)
	(h5new_tail s 0)
)

; helper function for above - h1 but tail recursive
; in the end - ended up slower
(defun h5new_tail (s accum)
	(cond ((null s) accum)
	      (t (h5new_tail (cdr s) (+ (count 2 (car s)) accum)))
	)
)
; a test heuristic - without the closest-box calculation
(defun h5normal (s)
	;(max
		(let* ((pos (getkeeperposition s 0))			;get keeper position
			 (kx (car pos))
			 (ky (cadr pos))
			 (bpos (getFirstBoxPosition s 0))		;get position of first box
			 (bx (car bpos))
			 (by (cadr bpos))
			 ;(by2 (if by by 0))
			 ;(gpos (getClosestGoalPosition (nthcdr by2 s) (reverseList (butlast s by2) nil) 0 by))		;get position of closest gate 
			 ;(gx (car gpos))
			 ;(gy (cadr gpos)))
			 (gx bx)
			 (gy by))
			 
			(if (and kx ky bx by gx gy)					; check if an answer was found
			    (+ (abs (- kx bx)) (abs (- ky by)) (abs (- bx gx)) (abs (- by gy))) ; calculate a sum
			    0							
			);end if
		);end let

		;(cond ((null s) 0)				; done searching through s, return 0
		;      (t (+ (count 2 (car s)) (h1 (cdr s)))) 	; count the number of boxes in this row, then continue
		;);end cond
	;);end max  
  
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
