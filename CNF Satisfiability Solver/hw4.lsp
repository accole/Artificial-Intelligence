; Adam Cole
; UID: 004912373
; HW4

;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

;benchmark tests to run for extra testing:
;https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (format-res n (dfs-back nil delta))
)


; HELPER FUNCTIONS


;dfs-back implements the backtracking needed to efficiently search
; the realm of potential answers

;Workflow

; choose a variable and assign a value
; remove ~var from all delta expressions and remove expressions made true
; backtrack if 0 legal values left in any expression

;inputs:
; curr - list of elements we are currently trying
; delta - current CNF expression
;output:
; nil if unsatisfiable CNF
; list of possible values if satisfiable CNF
(defun dfs-back (curr delta)
  (let ((f-list (car delta))  ;first list in CNF
	(next-sym (car (car delta)))) ;first elem in first list
    (cond
     ((null delta) curr)  ;base case - no more lists (found our answer!)
     ;check if alg should backtrack
     ((not (check-state delta)) nil)
     (t   ;else - check both possible paths
      (or (dfs-back (cons next-sym curr)  ;add variable to list
		    (update next-sym delta)) ;updated CNF expression
	  (dfs-back (cons (- next-sym) curr) ;add ~variable to list
		    (update (- next-sym) delta)))) ;updated CNF expression
     )
  )
)

;check-state tests if the dfs should return nil, backtracking to next
;path in the search.  iterates through delta and checks that there's no
;'nil's as expressions in the CNF.  If there is, return nil and backtrack.

;this is because when we remove the chosen variable from delta, if one of
; the "anded" clauses fail then we must backtrack.

; ex. '(() nil ()) since (...) & NIL & (...) returns false

;inputs:
; delta - current CNF expression, list of lists
;outputs:
; nil if alg should backtrack
; t if alg should continue
(defun check-state (delta)
  (let ((curr (car (car delta))))  ;first elem of current list
    (cond
     ((null delta) t) ;base case end of recursion
     ((null curr) nil) ;delta expression contains a nil as outer list member
     (t    ;else recurse
      (check-state (cdr delta)))
     )
  )
)



;update updates the CNF expression with the current value of sym

; updating does the following to delta:
; sym = 1 , delta = '((1 -2 -3) (-1) (-2 -3))  ->  '(nil (-2 -3))  BACKTRACK
; sym = -1 , delta = '((1 -2 -3) (-1) (-2 -3))  ->  '((-2 -3) (-2 -3))  CONTINUE

; update() removes each instance of -sym from delta in order to see
; the variables still affecting the satisfiability for those lists.
; if an interior list only has -sym as an element, this changes the
; expression to nil in delta.  This is our notification to backtrack

; update() also removes entire lists where +sym is present because this
; list has already evaluated to true.

;inputs:
; sym - symbol value to be updated throughout delta
; delta - list of lists CNF expression
;output:
; list of lists (updated CNF) with new sym value and replace lists that
; contain opposite value of sym occur.  This will help check for backtracking
(defun update (sym delta)
  (let ((expr (car delta)))  ;expr = first list in delta
    (cond
     ((null expr) nil)  ;base case no more expressions in delta
     ((not (check-list sym expr)) ;expr contains neither + or - sym
      (cons expr (update sym (cdr delta))))   ;don't modify list recurse to next one
     ((check-list-single sym expr)  ; + sym is in expr
      (update sym (cdr delta))) ;list with + sym returns true - remove from delta
     (t   ;else list contains - sym, remove sym from interior of list
      (cons (reduce-delt sym expr) ; removes -sym
	    (update sym (cdr delta)))) ;recurses
     )
  )
)



;reduce-delt removes -sym values from a delta expression

;ex. sym = 1, expr = (-1 2 3)  ->  (2 3)
;ex. sym = -1, expr = (1)  ->  nil

;inputs:
; sym - symbol to remove the -sym of
; expr - delta fragment to modify
;outputs:
; modifed list
(defun reduce-delt (sym expr)
  (let ((curr (car expr))  ;current symbol in expression
	(opp (- sym)))     ; ~sym
    (cond
     ((null expr) nil)  ;base case no elements left in expr
     ((= opp curr)    ;current element needs to be removed
      (reduce-delt sym (cdr expr)))  ;remove element and recurse
     (t  ;else keep curr and recurse
      (cons curr (reduce-delt sym (cdr expr))))
     )
    )
)



;format-res ensures the final expression has all variables included
; variables = 1-n
(defun format-res (n res)
  (cond
   ((null res) nil)  ;if dfs-back returns nil, nil
   ((= n 0) res)  ;base case no more numbers to check
   (t  ;else
    (cond
     ((check-list n res) (format-res (- n 1) res)) ;if var included, check next
     (t ;else add the "dont care" vars to make it a complete expression
      (format-res (- n 1) (cons n res)))
     ))
   )
)

;check-list checks if either the + or - variable is included in list
(defun check-list (var res)
  (let ((curr (car res)))
    (cond
     ((null res) nil) ;base case finished recursing
     ((= curr var) t) ;positive var
     ((= curr (- var)) t) ;or negative var
     (t  ;else recursive case
      (check-list var (cdr res)))
     )
    )
)

;differs from above only in the fact that check-list-single only
; searches for the positive var and not also the neg
(defun check-list-single (var res)
  (let ((curr (car res)))
    (cond
     ((null res) nil) ;base case finished recursing
     ((= curr var) t) ;positive var
     (t  ;else recursive case
      (check-list-single var (cdr res)))
     )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

