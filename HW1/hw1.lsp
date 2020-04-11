; Adam Cole
; UID: 004912373
; HW1


;                        Q1
; Tests if (integer) N appears in (ordered tree) TREE
; Inputs: N - integer, TREE - ordered tree as a list
; Output: boolean (t or nil)
(defun TREE-CONTAINS (N TREE) 
  (cond
   ((not TREE) nil)  ;base case when list is empty
   ((atom TREE) (= N TREE))  ;base case when just a number
   ((and (equal nil (cdr TREE)) (numberp (car TREE))) ;base case 1 element list
    (= N (car TREE)))
   ((and (equal nil (cdr TREE)) (list (car TREE)))
    (TREE-CONTAINS N (car TREE)))  ;recursive case for right subtree
   (t   ;recursive case where list has subtrees
        ;  test the head of the tree first
        ;  recursively check the subtrees
    (or (= N (cadr TREE))
	(TREE-CONTAINS N (car TREE))
	(TREE-CONTAINS N (cddr TREE))))
  )
)


;                        Q2
; Returns the minimum number present in (ordered tree) TREE
; Inputs: TREE - ordered tree as a list
; Output: minumum tree element as an integer
(defun TREE-MIN (TREE)
  (cond 
   ((atom TREE) TREE) ;base case - far left leaf of tree
   (t  ;recursive case, there's a left subtree with smaller vals
    (TREE-MIN (car TREE)))
   )
)


;                        Q3
; Returns a preorder list of elements in (ordered tree) TREE
; Inputs: TREE - ordered tree as a list
; Output: list of elements in preorder traversal, or nil if empty
(defun TREE-ORDER (TREE)
  (cond
   ((equal TREE nil) '())
   ((atom TREE) (cons TREE '())) ;base case tree is a number - return a list
   ((and (equal nil (cdr TREE)) (numberp (car TREE)))
    TREE)  ;base case list has 1 element - return as a list
   ((and (equal nil (cdr TREE)) (list (car TREE)))
    (TREE-ORDER (car TREE))) ;recursive case for right subtrees
   (t  ;recursive case where list has subtrees
       ;  preorder traversal, concatenate the return values together
    (let
	((left (TREE-ORDER (car TREE))) ;preorder list of left subtree
	 (right (TREE-ORDER (cddr TREE))) ;preorder list of right subtree
	 (head (cons (cadr TREE) '()))) ;head element as a list
        (append head left right) ;make one list of elements in order
    )
   )
  )
)


;                        Q4
; Returns a sublist of elements in L from start index START with length LEN
; Inputs: L - list, START - integer start index, LEN - integer length of sublist
; Output: list containing subset of consecutive elements in L
(defun SUB-LIST (L START LEN)
  (cond
   ((= LEN 0) '()) ;base case LEN is 0
   (t  ;recursive case
       ; recursively call (SUB_LIST(cdr L)(- START 1)LEN) until START = 0
       ; then cons recursively (SUB_LIST(cdr L)(0)(- LEN 1)) until LEN = 0
    (cond
     ((= START 0) ;creating sublist
      (cons (car L) (SUB-LIST (cdr L) 0 (- LEN 1))))
     (t  ; haven't reached START yet - remove first element and decrement index
      (SUB-LIST (cdr L) (- START 1) LEN))
    )
   )
  )
)


;                        Q5
; Returns a pair of lists equal (or differing by 1) in length
; that combine to form list L
; Inputs: L - list
; Output: (L1 L2) such that L1 and L2 combine to form L and they differ
;         in size by 0 or 1.
(defun SPLIT-LIST (L)
  (cond
   ((not L) nil) ;error handling if empty list
   ((atom L) (cons L nil)) ;base case list has length 1
   ((evenp (length L)) ;list has even length
                       ; split list directly in half
    (let
	((mid (/ (length L) 2))) ;store midpoint
        ;create L1 and L2 using SUB-LIST and create pair
        (list (SUB-LIST L 0 mid) (SUB-LIST L mid mid))
    ))
   ((oddp (length L)) ;list has odd length greater than 1
                      ; split list with a greater 1st half by 1
    (let
	((mid (/ (+ (length L) 1) 2))) ;store midpoint
        ;create L1 and L2 using SUB-LIST and create pair
        (list (SUB-LIST L 0 mid) (SUB-LIST L mid (- mid 1)))
    ))
  )
)


;                        Q6
; Returns the height of the binary tree TREE
; Inputs: TREE - Binary Search Tree as a list
; Output: height of the Binary Tree as an integer
(defun BTREE-HEIGHT (TREE)
  (cond
   ((atom TREE) 0)  ;leaf base case
   ((and (equal nil (cdr TREE)) (numberp (car TREE))) 0) ;leaf base case in list
   ((and (equal nil (cdr TREE)) (list (car TREE)))
    (BTREE-HEIGHT (car TREE))) ; recursive case internal node subtree
   (t  ;recursive case internal node
    (let
	((left_height (BTREE-HEIGHT (car TREE))) ;height of left subtree
	 (right_height (BTREE-HEIGHT (cdr TREE)))) ;height of right subtree
	(cond
	 ((< left_height right_height) ; left < right ... return right + 1
	  (+ 1 right_height))
	 (t  ; else right <= left ... return left + 1
	  (+ 1 left_height))
	 ))
    )
   )
)


;                        Q7
; Returns a balanced Binary Search Tree containing the atoms in list LEAVES
; Inputs: LEAVES - nonempty list of atoms
; Output: binary search tree such that the leaves are the elements
;         in LEAVES and the tree is as balanced as possible
(defun LIST2BTREE (LEAVES)
  (cond
   ((atom LEAVES) LEAVES) ;base case 1 element
   ((and (equal nil (cdr LEAVES)) (numberp (car LEAVES)))
    (car LEAVES)) ;base case right subtree with 1 element
   ((and (equal nil (cdr LEAVES)) (list (car LEAVES)))
    (LIST2BTREE (car LEAVES))) ;recursive case for right subtree
   (t ;recursive case
    (let
	((right_list (cdr (SPLIT-LIST LEAVES))) ;right atoms from LEAVES
	 (left_list (car (SPLIT-LIST LEAVES)))) ;left atoms from LEAVES
        (list (LIST2BTREE left_list) (LIST2BTREE right_list)) ;combine subtrees
	))
   )
)


;                        Q8
; Inverse function of LIST2BTREE
; Inputs: TREE - Binary Search Tree
; Output: List of atoms in TREE in order
(defun BTREE2LIST (TREE)
  (cond
   ((equal TREE '()) '()) ;if nil, return nil
   ((atom TREE) (cons TREE '())) ;base case 1 element return as a list
   ((and (equal nil (cdr TREE)) (numberp (car TREE)))
    (cons (car TREE) '()))  ;base case right subtree with 1 element
   ((and (equal nil (cdr TREE)) (list (car TREE)))
    (BTREE2LIST (car TREE)))  ;recursive case for right subtree
   (t  ;recursive case
    (let
	((right_leaves (BTREE2LIST (cdr TREE))) ;right subtree list
	 (left_leaves (BTREE2LIST (car TREE)))) ;left subtree list
        (append left_leaves right_leaves) ;combines lists with append
	))
   )
)


;                        Q9
; Compares two lisp expressions E1 and E2 and checks if they are identical
; Inputs: E1, E2 - lisp expressions whose atoms are all numbers
; Output: equivalence as a boolean (t or nil)
(defun IS-SAME (E1 E2)
  (cond
   ((and (null E1) (null E2)) t) ;base case both expressions are nil
   ((and (atom E1) (atom E2))  ;base case both = atoms then test equivalence
    (if (= E1 E2) t nil))
   ((let                          ;base cases for right subtree cases
	((e1_check (and (null (cdr E1)) (not (null (car E1)))))
	 (e2_check (and (null (cdr E2)) (not (null (car E1))))))
        (and e1_check e1_check))
    (IS-SAME (car E1) (car E2)))
   ((= (length E1) (length E2))  ;recursive case both lists must have equal length
    (let
	((car_equiv (IS-SAME (car E1) (car E2))) ;equivalence check for car
	 (cdr_equiv (IS-SAME (cdr E1) (cdr E2)))) ;equivalence check for cdr
        (and car_equiv cdr_equiv)  ;must both be true
	))
   (t nil) ;default case nil
   )
)
