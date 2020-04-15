; Adam Cole
; UID: #########
; HW2


;                        Q1
; Breadth First Search - Left to Right
; Input : TREE - a tree with varying branching factor as a list
; Output : A list of the Breadth First traversal
(defun BFS (TREE)
  (cond
   ((equal TREE nil) '())  ;base case done with traversal
   ((atom (car TREE)) ;if the next element in traversal is an atom
    ;add it to the list and recursively call the rest of the TREE
    (cons (car TREE) (BFS (cdr TREE))))
   ((list (car TREE)) ;recursive case - next element is a list
    ;append the list to the back of the tree and recurse to next element
    (BFS (append (cdr TREE) (car TREE))))
   )
)


;                        Q2
; Depth First Search - Right to Left
; Input : TREE - a tree with varying branching factor as a list
; Output : A list of the Breadth First traversal
(defun DFS (TREE)
  (cond
   ((equal TREE nil) '())  ;base case end of list
   ((atom TREE) (cons TREE '()))  ;base case left subtree leaf
   ((and (null (cdr TREE)) (not (null (car TREE))))  ;right subtree
    (DFS (car TREE)))
   (t      ;recursive case - internal nodes
    (let
	((left (DFS (car TREE)))  ;left subtree recursive call
	 (right (DFS (cdr TREE)))) ;right subtree recursive call
        (append right left)))
   )
)


;                        Q3
; Depth First Iterative Deepening Search - Left to Right
; Input : TREE - a tree with varying branching factor as a list
; Output : A list of the Breadth First traversal
(defun DFID (TREE max)
  (iter_deepen TREE max 0)  ;call helper functions
)

;helper function to control iterative max depth
(defun iter_deepen (TREE max curr)
  (cond 
   ((> curr max) '())  ;if greater than max depth append nil
   ((null TREE) '())  ;argument testing
   (t     ;else
    (append (df_trav TREE curr 0)  ;get the DFID traversal list
	  (iter_deepen TREE max (+ curr 1)))) ;append it to next greater depth
   )
)

;helper function to return DFS traversal list to a max depth
(defun df_trav (TREE max curr)
  (cond
   ((> curr max) '()) ;else if deeper than max return nil
   ((null TREE) '())  ;base cases
   ((atom TREE) (cons TREE '()))
   (t                 ;do depth first traversal, only increment when deepening
    (append (df_trav (car TREE) max (+ 1 curr)) (df_trav (cdr TREE) max curr)))
   )
)


;                        Q4

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (let ((final (list 3 3 nil)))  ;test if s equals goal state
    (if (equal s final) t nil)
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
; s = (m c t/nil)
; m = # of missionaries to move
; c = # of cannibals to move
; m or c can't be negative on either side
; m can't be < c unless m = 0
; must calculate for both sides
(defun next-state (s m c)
  (let 
      ((m_next (- (car s) m))  ;current side next # of missionaries
       (c_next (- (cadr s) c)) ;current side next # of cannibals
       (m_other (+ (- 3 (car s)) m)) ;other side # of missionaries
       (c_other (+ (- 3 (cadr s)) c)) ;other side # of cannibals
       (boat (caddr s)))   ;current side of boat
    (cond  ;test values of m and c
     ((or (> 0 m_next) (> 0 c_next) (> 0 m_other) (> 0 c_other)) ;if any are negative
      nil)  ;return nil
     ((and (> c_next m_next) (not (= m_next 0))) ;if cannibals > missionaries
      nil)  ;return nil
     ((and (> c_other m_other) (not (= m_other 0))) ;if cann. > miss. on other side
      nil)  ;return nil
     (t  ;else valid state, return a list like ((3 3 nil))
      (list (list m_other c_other (not boat))))
     )
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; all possible moves:
; next-state(s 0 1)
; next-state(s 0 2)
; next-state(s 1 0)
; next-state(s 1 1)
; next-state(s 2 0)
(defun succ-fn (s)
  (append (next-state s 0 1) (next-state s 0 2) ;creates a list of all poss next states
	  (next-state s 1 0) (next-state s 1 1) (next-state s 2 0))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond
   ((null states) nil)  ;base case if states is empty
   ((equal s (car states)) t)  ;if s is the first element
   (t  ;recursive case
    (on-path s (cdr states)))  ;check rest of stack
   )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond
   ((null states) nil)  ;base case if states empty then nil
   (t   ;recursive case
    (let 
	((df (mc-dfs (car states) path))) ;dfs of first possible state
      (cond
       ((null df) (mult-dfs (cdr states) path)) ;if nil, search next states
       ((list df) df))))  ;if dfs returns a list, reached final state
   )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond
   ((null s) nil)  ;base case s is nil
   ((final-state s) (cons s path))  ;if s = final state attach and return path
   ((on-path s path) nil)     ;if s is on path return nil 
   (t                             ;default
    (mult-dfs (succ-fn s) (cons s path))) ;call helper func with correct args
   )
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
