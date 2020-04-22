;  Adam Cole
;  UID: #########
;  HW3
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

;(defun isKeeper (v)
;  (= v keeper)
;  )

(defun iskeeper (char)
  (cond
   ((= char 3) t) ; keeper
   ((= char 6) t) ; keeper on goal state
   (t nil) ; else false
   )
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
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; check whole grid
; no '$' on grid (boxes not on goal square)
; no '@' on grid (sokoban must also be on goal square)
(defun goal-test (s)
  (cond
   ((null s) t)  ;base case no more rows
   ((atom s) (list_helper s))
   ((and (null (cdr s)) (not (null (car s))))
    (list_helper (car s)))
   (t   ;recursive case
    (and (list_helper (car s)) (goal-test (cdr s))))
   )
);end defun

;tests a single list
(defun list_helper (l)
  (cond
   ((null l) t)  ;base case null tree
   ((atom l) (test l))  ;base case call helper func
   ((and (null (cdr l)) (not (null (car l))))     ;base case right subtree
    (list_helper (car l)))    ;recurse
   (t     ;recursive case
    (and (test (car l)) (list_helper (cdr l))))
   )
)

;helper function for goal-test - checks if current atom is @ or $
; returns false if the character is @ or $
; true otherwise
(defun test (char)
  (cond
   ((= char 2) nil)  ;$ - box not on goal square
   ((= char 3) nil)  ;@ - sokoban not on goal square
   (t t)   ;default case
   )
)

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

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos)))
	 ;x and y are now the coordinate of the keeper in s.
    (cleanUpList (list (try-move x y s 1) ;up
		       (try-move x y s 2) ;down
		       (try-move x y s 3) ;left
		       (try-move x y s 4))) ;right
   );end let
  );

; helper function to create next states
; returns nil if invalid, or next state otherwise
(defun try-move (x y s dir)
  (cond
   ((= dir 1)  ;up
    (let ((res (up x y s)))   ;res = valid move test
      (if (null res) nil (create-u x y s))))  ;if valid, create next state otherwise nil
   ((= dir 2)  ;down
    (let ((res (down x y s)))   ;res = valid move test
      (if (null res) nil (create-d x y s))))  ;if valid, create next state otherwise nil
   ((= dir 3)  ;left
    (let ((res (left y s)))   ;res = valid move test
      (if (null res) nil (create-l x y s))))  ;if valid, create next state otherwise nil
   ((= dir 4)  ;right
    (let ((res (right x y s)))   ;res = valid move test
      (if (null res) nil (create-r x y s))))  ;if valid, create next state otherwise nil
   )
)


;;;;;;;;;;;;;;;;;;;;;;  LEFT / RIGHT VALID FUNCTIONS  ;;;;;;;;;;;;;;;;;;;

(defun right (x y s)
  (try-right x (find-row y s))
)

(defun left (y s)
  (try-left (find-row y s))
)

; helper function for next-state
; returns the single list for try left and right moves
(defun find-row (y s)
  (cond
   ((null s) nil)  ;no more rows - should never happen
   ((= 0 y) (car s)) ;at the correct row - return row
   (t   ;else
    (find-row (- y 1) (cdr s)))  ;recurse to find row
   )
)

; return t if valid move or nil if not valid
; given the correct row, get to correct position and test elements
(defun try-left (row)
  (cond
   ((null row) nil)  ;base case no more elements - shouldn't happen
   ((iskeeper (car row)) nil)  ;sokoban index 0 in the list - always false bc go off map
   ((iskeeper (cadr row))   ;index 1 in row - only check car
    (if (= 0 (car row)) t nil))
   ((iskeeper (caddr row))  ;2 left of sokoban location 
    (cond
     ((= 0 (cadr row)) t)  ;next space is open space - return true
     ((= 1 (cadr row)) nil)  ;next space is a wall - return nil
     ((= 2 (cadr row)) (box-test row)) ;next space is a box - check following space
     ((= 4 (cadr row)) t)  ;next space is goal space - return true
     ((= 5 (cadr row)) (box-test row)) ;;next space is a box - check following space
     ))
   (t   ;else
    (try-left (cdr row)))  ;not yet at sokoban - recurse
   )
)

; helper function for testing left box cases
; car row = ?
; cadr row = box
; return true if box can be pushed, false otherwise
(defun box-test (row)
  (cond
   ((null row) nil)  ;if edge of board nil
   ((= 0 (car row)) t)  ;next space is open space - return true
   ((= 4 (car row)) t)  ;next space is goal space - return true
   (t nil)  ;else nil
   )
)

; return t if valid move or nil if not valid
; given the correct row, get to correct position and test elements
(defun try-right (x row)
  (cond
   ((= (+ x 1) (length row)) nil) ;x is far right - can't move right
   ((null row) nil)  ;base case no more elements - shouldn't happen
   ((> x 0) (try-right (- x 1) (cdr row)))  ;not yet at sokoban - recurse
   ((and (= x 0) (iskeeper (car row)))  ;at the sokoban location 
    (cond
     ((= 0 (cadr row)) t)  ;next space is open space - return true
     ((= 1 (cadr row)) nil)  ;next space is a wall - return nil
     ((= 2 (cadr row)) (box-test (cddr row))) ;next space is a box - check following space
     ((= 4 (cadr row)) t)  ;next space is goal space - return true
     ((= 5 (cadr row)) (box-test (cddr row))) ;;next space is a box - check following space
     (t nil)  ;anything else nil
     ))
   )
)


;;;;;;;;;;;;;;;;;  UP / DOWN VALID FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;


; helper function for try up
; subtract 2 from y because we need 2 rows on top of y as well
(defun up (x y s)
  (cond
   ((= y 0) nil)  ;can't move up from row 0
   ((= y 1) (find-rows-up x y s 2))  ;only use 1 column up
   (t   ;else
    (find-rows-up x (- y 2) s 3))  ;use 2 columns above
   )
)

; helper function - finds the 3 rows we will use and passes
; them into try-up
(defun find-rows-up (x y s rows)
  (cond
   ((= rows 2)  ;use row 0 and 1
    (try-up2 x (car s) (cadr s)))
   ((= rows 3)
    (cond
     ((= y 0)  ;reached top list
      (try-up3 x (car s) (cadr s) (caddr s)))
     (t  ;recursive case
      (find-rows-up x (- y 1) (cdr s) rows))
     ))
   )
)

; x is x position
; l1 is top list (index 0)
; l2 is bottom list (index 1)
(defun try-up2 (x l1 l2)
  (cond
   ((> x 0) (try-up2 (- x 1) (cdr l1) (cdr l2)))  ;not at keeper yet
   ((and (= x 0) (iskeeper (car l2)))  ;reached the correct position
    (cond
     ((= (car l1) 0) t)  ;open space
     ((= (car l1) 1) nil)  ;wall
     ((= (car l1) 2) nil)  ;box up against wall
     ((= (car l1) 4) t)  ;goal space
     ((= (car l1) 5) nil)  ;box up against wall
     (t nil)  ;anything else nil
     ))
   )
)

; x is x position
; l1 is top list (index 0)
; l2 is middle list (index 1)
; l3 is bottom list (index 2)
(defun try-up3 (x l1 l2 l3)
  (cond
   ((> x 0) (try-up3 (- x 1) (cdr l1) (cdr l2) (cdr l3)))  ;not at keeper yet
   ((and (= x 0) (iskeeper (car l3)))  ;reached the correct position
    (cond
     ((= (car l2) 0) t)  ;open space
     ((= (car l2) 1) nil)  ;wall
     ((= (car l2) 2) (box-test l1))  ;box up
     ((= (car l2) 4) t)  ;goal space
     ((= (car l2) 5) (box-test l1))  ;box on goal space
     (t nil)  ;anything else nil
     ))
   )
)

(defun down (x y s)
  (let ((maxrow (- (length s) 1)))
    (cond
     ((= y maxrow) nil)  ;can't move down from last row
     ((= y (- maxrow 1)) (find-rows-down x y (nthcdr y s) 2))  ;only use 1 column above
     ;((= y 0) (find-rows-down x 0 s 3))
     (t   ;else
      (find-rows-down x 0 (nthcdr y s) 3))))  ;use 2 columns above
)

(defun find-rows-down (x y s rows)
  (cond
   ((= rows 2)  ;use row maxrow-1 and maxrow
    (try-down2 x (car s) (cadr s)))
   ((= rows 3)
    (cond
     ((= y 0)  ;reached top list
      (try-down3 x (car s) (cadr s) (caddr s)))
     (t  ;recursive case
      (find-rows-down x (- y 1) (cdr s) rows))
     ))
   )
)

; x is x position
; l1 is top list (index maxrow - 1)
; l2 is bottom list (index maxrow)
(defun try-down2 (x l1 l2)
  (cond
   ((> x 0) (try-down2 (- x 1) (cdr l1) (cdr l2)))  ;not at keeper yet
   ((and (= x 0) (iskeeper (car l1)))  ;reached the correct position
    (cond
     ((= (car l2) 0) t)  ;open space
     ((= (car l2) 1) nil)  ;wall
     ((= (car l2) 2) nil)  ;box up against wall
     ((= (car l2) 4) t)  ;goal space
     ((= (car l2) 5) nil)  ;box up against wall
     (t nil)  ;anything else nil
     ))
   )
)

; x is x position
; l1 is top list (index ~0)
; l2 is middle list (index ~1)
; l3 is bottom list (index ~2)
(defun try-down3 (x l1 l2 l3)
  (cond
   ((> x 0) (try-down3 (- x 1) (cdr l1) (cdr l2) (cdr l3)))  ;not at keeper yet
   ((and (= x 0) (iskeeper (car l1)))  ;reached the correct position
    (cond
     ((= (car l2) 0) t)  ;open space
     ((= (car l2) 1) nil)  ;wall
     ((= (car l2) 2) (box-test l3))  ;box down
     ((= (car l2) 4) t)  ;goal space
     ((= (car l2) 5) (box-test l3))  ;box on goal space
     (t nil)  ;anything else nil
     ))
   )
)


;;;;;;;;;;;;;;;;;;  CREATE FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;


;RIGHT

;find the row, update the 1 row by:
;  1.  get position using coordinates
;  2.  use butlast up to (danger zone) to get the first part
;  3.  splice in the correct sequence with cond statement
;  4.  use nthcdr from (danger zone) to get the last part
;list (lists) all the rows to form the new map

; sets up row appending for right moves
; butlast s (- (length s) y) returns lists up to yth row
; nthcdr (+ y 1) s returns all lists after yth row
; car (nthcdr y s) returns list y
(defun create-r (x y s)
  (cond
   ((= y 0)  ;y is in the first row
    (append (list (move-r x (car s)))  ;changed list
	    (nthcdr (+ y 1) s))) ;append to the rest
   ((= y (length s)) ; y is in the last row
    (append (butlast s)  ; all rows but last
	    (list (move-r x (nthcdr y s))))) ;combine with changed one
   (t       ; else
    (append (butlast s (- (length s) y)) ;prior lists
	    (list (move-r x (car (nthcdr y s))))  ; changed list
	    (nthcdr (+ y 1) s)))  ;remaining lists
   )
)

; sets up element appending within a row for right moves
; butlast row (- (length row) x) returns elements up to xth col
; nthcdr (+ x 1) row returns all elements after xth col
; therefore nthcdr (+ x 2) row returns all elements after empty space moves
; and nthcdr (+ x 3) row returns all elements after box pushes
(defun move-r (x row)
  (let ((maxcol (- (length row) 2))
	(size (newsize x row)))
    (cond
     ((= x 0)  ;sokoban @ 0 - no prior elements
      (append (rmove x row size)  ;new elements of size size
	      (nthcdr (+ x size) row)))  ;unedited following elements
     ((= x maxcol) ; sokoban @ far right - 1, no after elements
      (append (butlast row (- (length row) x)) ;unedited prior elements
	      (rmove x row size)))  ;new elements of size size
     (t    ;else - prior, changed elements, and after.
      (append (butlast row (- (length row) x)) ;unedited prior elements
	      (rmove x row size)  ;new elements of size size
	      (nthcdr (+ x size) row))) ;unedited following elements (might be none)
     )
  )
)

;returns true if char is a box, nil otherwise
(defun isbox (char)
  (or (= char 2) (= char 5))
)

; returns how big the edited size is
; -> 3 if a box push, 2 if open space move
(defun newsize (x row)
  (if (isbox (car (nthcdr (+ x 1) row))) 3 2) ;if next element is a box
)

;possible box push moves:
;(3 2 0) -> (0 3 2)     no goal
;(3 2 4) -> (0 3 5)     (0 0 G)
;(3 5 0) -> (0 6 2)     (0 G 0)
;(6 2 0) -> (4 3 2)     (G 0 0)
;(6 5 0) -> (4 6 2)     (G G 0)
;(6 2 4) -> (4 3 5)     (G 0 G)
;(3 5 4) -> (0 6 5)     (0 G G)
;(6 5 4) -> (4 6 5)     (G G G)

;possible open space moves:
;(3 0) -> (0 3)         no goal
;(3 4) -> (0 6)         (0 G) 
;(6 0) -> (4 3)         (G 0)
;(6 4) -> (4 6)         (G G)
(defun rmove (x row size)
  (let ((after (nthcdr x row))  ;list of elements from sokoban
	(pos (car (nthcdr x row))) ;sokoban encoding
	(next (car (nthcdr (+ x 1) row)))) ;next number after sokoban
    (cond
     ((and (= pos 3) (= next 0))  ;open space moves
      (list 0 3))
     ((and (= pos 3) (= next 4))
      (list 0 6))
     ((and (= pos 6) (= next 0))
      (list 4 3))
     ((and (= pos 6) (= next 4))
      (list 4 6))
     ((and (= pos 3) (= next 2) (= (caddr after) 0))  ;box pushes
      (list 0 3 2))
     ((and (= pos 3) (= next 2) (= (caddr after) 4))
      (list 0 3 5))
     ((and (= pos 3) (= next 5) (= (caddr after) 0))
      (list 0 6 2))
     ((and (= pos 6) (= next 2) (= (caddr after) 0))
      (list 4 3 2))
     ((and (= pos 6) (= next 5) (= (caddr after) 0))
      (list 4 6 2))
     ((and (= pos 6) (= next 2) (= (caddr after) 4))
      (list 4 3 5))
     ((and (= pos 3) (= next 5) (= (caddr after) 4))
      (list 0 6 5))
     ((and (= pos 6) (= next 5) (= (caddr after) 4))
      (list 4 6 5))
     )
    )
)


;LEFT

; sets up row appending for left moves
; butlast s (- (length s) y) returns lists up to yth row
; nthcdr (+ y 1) s returns all lists after yth row
; car (nthcdr y s) returns list y
(defun create-l (x y s)
  (cond
   ((= y 0)  ;y is in the first row
    (append (list (move-l x (car s)))  ;changed list
	    (nthcdr (+ y 1) s))) ;append to the rest
   ((= y (length s)) ; y is in the last row
    (append (butlast s)  ; all rows but last
	    (list (move-l x (nthcdr y s))))) ;combine with changed one
   (t       ; else
    (append (butlast s (- (length s) y)) ;prior lists
	    (list (move-l x (car (nthcdr y s))))  ; changed list
	    (nthcdr (+ y 1) s)))  ;remaining lists
   )
)


; sets up element appending within a row for left moves
; butlast row (- (length row) x) returns elements up to xth col
; nthcdr (+ x 1) row returns all elements after xth col
; therefore nthcdr (+ x 2) row returns all elements after empty space moves
; and nthcdr (+ x 3) row returns all elements after box pushes
(defun move-l (x row)
  (let ((maxcol (- (length row) 1))
	(index (- (length row) (+ (- x (newsize_l x row)) 1)))
	(size (newsize_l x row)))
    (cond
     ((= x 1)  ;sokoban @ 1 - no prior elements
      (append (lmove x row size)  ;new elements of size size
	      (nthcdr size row)))  ;unedited following elements
     ((= x maxcol) ; sokoban @ far right, no after elements
      (append (butlast row size) ;unedited prior elements
	      (lmove x row size)))  ;new elements of size size
     (t    ;else - prior, changed elements, and after.
      (append (butlast row index) ;unedited prior elements (might be none)
	      (lmove x row size)  ;new elements of size size
	      (nthcdr (+ x 1) row))) ;unedited following elements
     )
  )
)

; returns how big the edited size is
; -> 3 if a box push, 2 if open space move
(defun newsize_l (x row)
  (if (isbox (car (nthcdr (- x 1) row))) 3 2) ;if next element is a box
)

;possible box push moves:
;(0 2 3) -> (2 3 0)     no goal
;(4 2 3) -> (5 3 0)     (G 0 0)
;(0 5 3) -> (2 6 0)     (0 G 0)
;(0 2 6) -> (2 3 4)     (0 0 G)
;(0 5 6) -> (2 6 4)     (0 G G)
;(4 2 6) -> (5 3 4)     (G 0 G)
;(4 5 3) -> (5 6 0)     (G G 0)
;(4 5 6) -> (5 6 4)     (G G G)

;possible open space moves:
;(0 3) -> (3 0)         no goal
;(4 3) -> (6 0)         (G 0) 
;(0 6) -> (3 4)         (0 G)
;(4 6) -> (6 4)         (G G)
(defun lmove (x row size)
  (let ((pos (car (nthcdr x row))) ;sokoban encoding
	(next (car (nthcdr (- x 1) row)))) ;next number after sokoban
    (cond
     ((and (= pos 3) (= next 0))  ;open space moves
      (list 3 0))
     ((and (= pos 3) (= next 4))
      (list 6 0))
     ((and (= pos 6) (= next 0))
      (list 3 4))
     ((and (= pos 6) (= next 4))
      (list 6 4))
     ((and (= pos 3) (= next 2) (= (car (nthcdr (- x 2) row)) 0))  ;box pushes
      (list 2 3 0))
     ((and (= pos 3) (= next 2) (= (car (nthcdr (- x 2) row)) 4))
      (list 5 3 0))
     ((and (= pos 3) (= next 5) (= (car (nthcdr (- x 2) row)) 0))
      (list 2 6 0))
     ((and (= pos 6) (= next 2) (= (car (nthcdr (- x 2) row)) 0))
      (list 2 3 4))
     ((and (= pos 6) (= next 5) (= (car (nthcdr (- x 2) row)) 0))
      (list 2 6 4))
     ((and (= pos 6) (= next 2) (= (car (nthcdr (- x 2) row)) 4))
      (list 5 3 4))
     ((and (= pos 3) (= next 5) (= (car (nthcdr (- x 2) row)) 4))
      (list 5 6 0))
     ((and (= pos 6) (= next 5) (= (car (nthcdr (- x 2) row)) 4))
      (list 5 6 4))
     )
    )
)


;UP

; sets up row appending for up moves
; append before, edited row, and after rows
(defun create-u (x y s)
  (let ((size (newsize-u x y s))
	(maxrow (- (length s) 1)))
    (cond
     ((= y 1) ;sokoban in 2nd row, no before lists
      (append (move-u x y size (butlast s (- maxrow y)))
	      (nthcdr 2 s)))  ;after lists
     ((= y maxrow) ;sokoban in last row, no after lists
      (move-u x y size s)) 
     (t   ; else sokoban in middle - attach before, changed, after
      (append (move-u x y size (butlast s (- maxrow y)))
	      (nthcdr (+ y 1) s)))  ;after lists                                      
     )
  )
)

;recursive calling to update rows for up moves
(defun move-u (x y size s)
  (cond
  ((null s) nil) ;base case no more rows
  ((= y (- size 1))  ;begin to change the rows
   (cond
    ((= size 2)  ;open space
     (let ((lt (car s))
	   (lb (cadr s))
	   (top (getelem x (car s)))
	   (bottom (getelem x (cadr s))))
       (cond
	((and (= top 0) (= bottom 3))   ;all cases
	 (append (list (replace_row x lt 3))
		 (list (replace_row x lb 0))))
	((and (= top 4) (= bottom 3))
	 (append (list (replace_row x lt 6))
		 (list (replace_row x lb 0))))
	((and (= top 0) (= bottom 6))
	 (append (list (replace_row x lt 3))
		 (list (replace_row x lb 4))))
	((and (= top 4) (= bottom 6))
	 (append (list (replace_row x lt 6))
		 (list (replace_row x lb 4))))
	)))
    ((= size 3)  ;box push
     (let ((lt (car s))
	   (lm (cadr s))
	   (lb (caddr s))
	   (top (getelem x (car s)))
	   (mid (getelem x (cadr s)))
	   (bottom (getelem x (caddr s))))
       (cond
	((and (= top 0) (= mid 2) (= bottom 3))  ;all cases
	 (append (list (replace_row x lt 2))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 0))))
	((and (= top 4) (= mid 2) (= bottom 3))
	 (append (list (replace_row x lt 5))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 0))))
	((and (= top 0) (= mid 5) (= bottom 3))
	 (append (list (replace_row x lt 2))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 0))))
	((and (= top 0) (= mid 2) (= bottom 6))
	 (append (list (replace_row x lt 2))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 4))))
	((and (= top 0) (= mid 5) (= bottom 6))
	 (append (list (replace_row x lt 2))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 4))))
	((and (= top 4) (= mid 2) (= bottom 6))
	 (append (list (replace_row x lt 5))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 4))))
	((and (= top 4) (= mid 5) (= bottom 3))
	 (append (list (replace_row x lt 5))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 0))))
	((and (= top 4) (= mid 5) (= bottom 6))
	 (append (list (replace_row x lt 5))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 4))))
	)
       ))
    ))
  (t   ;else append row and recurse
   (append (list (car s))
	   (move-u x (- y 1) size (cdr s))))
))

;replaces an element in row at column x to element e
(defun replace_row (x row e)
  (let ((prior (butlast row (- (length row) x)))
	(changed (list e))
	(after (nthcdr (+ x 1) row)))
    (append prior changed after))
)

; returns 3 if box push, 2 otherwise
(defun newsize-u (x y s)
  (let ((next_row (car (nthcdr (- y 1) s)))) ;row above sokoban
    (if (isbox (getelem x next_row)) 3 2) ;if next element is a box
  )
)

; helper function to return element of a row at position x
(defun getelem (x row)
  (car (nthcdr x row))
)



;DOWN

; sets up row appending for down moves
; append before, edited row, and after rows
(defun create-d (x y s)
  (let ((size (newsize-d x y s))
	(maxrow (- (length s) 2)))
    (cond
     ((= y maxrow)        ;sokoban in last row, no after lists
      (move-d x y size s))                                   ;g 
     (t   ; else sokoban in middle - attach changed, after
      (append (move-d x y size (butlast s (- (length s) (+ y size))))
	      (nthcdr (+ y size) s)))  ;after lists - could be zero      
     )
  )
)


; returns 3 if box push, 2 otherwise
(defun newsize-d (x y s)
  (let ((next_row (car (nthcdr (+ y 1) s)))) ;row after sokoban
    (if (isbox (getelem x next_row)) 3 2) ;if next element is a box
  )
)


;recursive calling to update rows for down moves
(defun move-d (x y size s)
  (cond
  ((null s) nil) ;base case no more rows
  ((= y 0)  ;begin to change the rows
   (cond
    ((= size 2)  ;open space
     (let ((lt (car s))
	   (lb (cadr s))
	   (top (getelem x (car s)))
	   (bottom (getelem x (cadr s))))
       (cond
	((and (= top 3) (= bottom 0))   ;all cases
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lb 3))))
	((and (= top 3) (= bottom 4))
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lb 6))))
	((and (= top 6) (= bottom 0))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lb 3))))
	((and (= top 6) (= bottom 4))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lb 6))))
	)))
    ((= size 3)  ;box push
     (let ((lt (car s))
	   (lm (cadr s))
	   (lb (caddr s))
	   (top (getelem x (car s)))
	   (mid (getelem x (cadr s)))
	   (bottom (getelem x (caddr s))))
       (cond
	((and (= top 3) (= mid 2) (= bottom 0))  ;all cases
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 2))))
	((and (= top 3) (= mid 2) (= bottom 4))
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 5))))
	((and (= top 3) (= mid 5) (= bottom 0))
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 2))))
	((and (= top 6) (= mid 2) (= bottom 0))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 2))))
	((and (= top 6) (= mid 5) (= bottom 0))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 2))))
	((and (= top 6) (= mid 2) (= bottom 4))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lm 3))
		 (list (replace_row x lb 5))))
	((and (= top 3) (= mid 5) (= bottom 4))
	 (append (list (replace_row x lt 0))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 5))))
	((and (= top 6) (= mid 5) (= bottom 4))
	 (append (list (replace_row x lt 4))
		 (list (replace_row x lm 6))
		 (list (replace_row x lb 5))))
	)
       ))
    ))
  (t   ;else append row and recurse
   (append (list (car s))
	   (move-d x (- y 1) size (cdr s))))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; returns constant 0
(defun h0 (s) 0 )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; This is an admissible heuristic because h1 counts the number
; of misplaced boxes in the game.  In Sokoban, the smaller this
; number is, the closer the algorithm is to reaching success.
; Therefore, h1 is admissible.
;
(defun h1 (s)
  (cond
   ((null s) 0) ;; end of rows - return 0
   ((and (null (cdr s)) (not (null (car s)))) ;right subtree
    (box_counter (car s)))  ;count number of 2's
   (t  ;recursive case
    (+ (box_counter (car s)) (h1 (cdr s))))  ;add row results to recursive call
   )
)

(defun box_counter (row)
  (cond
   ((null row) 0)  ;;end of row - return 0
   ((atom row)    ;;element of the list - return 1 if box
    (if (= row 2) 1 0))
   ((and (null (cdr row)) (not (null (car row))))  ;right subtree
    (box_counter (car row)))
   (t   ;recursive case
    (+ (box_counter (car row)) (box_counter (cdr row))))
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

; Ran out of time writing this project.
; I finished implementing the functionality of the game on Tuesday night, 
; so I had to omit writing my own heuristic function.  If I had left more
; time for this, I would use manhattan distance to find the shortest path
; between the sokoban and the nearest box, and sokoban and the nearest goal state.
;
(defun hUID (s) 0 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;stack overflow on this question
;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;runs forever
;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

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
