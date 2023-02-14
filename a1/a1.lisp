; Logan Vaughan
; 1577489
; CMPUT 325
; LEC B1
; Assignment 1

; QUESTION 1 ----------------------------------------------------------------------------
; occurs: returns the number of times the atom E occurs in the nested list L
(defun occurs (E L)
    (cond
        ((null L) 0)
        ((not (atom (car L))) (+ (occurs E (car L)) (occurs E (cdr L))))
        ((eq (car L) E) (+ 1 (occurs E (cdr L))))
        (T (occurs E (cdr L)))
    )
)

; QUESTION 2 ----------------------------------------------------------------------------
; xflatten: returns a flat list from the nested list x
(defun xflatten (x)
    (cond
        ((null x) nil)
        ((not (atom (car x))) (append (xflatten (car x)) (xflatten (cdr x))))
        (T (cons (car x) (xflatten (cdr x))))
    )
)

; QUESTION 3 ----------------------------------------------------------------------------
; remove-duplicate: returns the list x with duplicate atoms removed
(defun remove-duplicate (x)
    (cond
        ((null x) nil)
        ((eq (occurs (car x) (cdr x)) 0) (cons (car x) (remove-duplicate (cdr x))))
        (T (remove-duplicate (cdr x)))
    )
)

; QUESTION 4a ---------------------------------------------------------------------------
; mix: returns a single list with alternating items from L1 and L2
; test case: (mix '(a b) '(c d e f)) => (c a d b e f)
(defun mix (L1 L2)
    (if (null L1)
        (if (null L2)
            nil
            L2
        )
        (if (null L2)
            L1
            (cons (car L2) (mix (cdr L2) L1))
        )
    )
)

; QUESTION 4b ---------------------------------------------------------------------------
; split: returns two lists with the even and odd indexed items of L
; test case: (split '(a b c d e)) => ((b d) (a c e))
(defun split (L)
    (cond
        ((null L) (list nil nil))
        ((null (cdr L)) (list nil (list (car L))))
        (T (join (list (list (cadr L)) (list (car L))) (split (cddr L))))
    )
)
; join: returns a list of two lists from the joined values L1 and L2 (L1 and L2 are lists
;       of two lists containing one item each)
; test case: (join '((1) (2)) '((3) (4))) => ((1 3) (2 4))
(defun join (L1 L2)
    (if (null L1)
        (if (null L2)
            nil
            (list (car L2) (cadr L2))
        )
        (if (null L2)
            (list (car L1) (cadr L1))
            (list (append (car L1) (car L2)) (append (cadr L1) (cadr L2)))
        )
    )
)

; QUESTION 5 ----------------------------------------------------------------------------
; allsubsets: returns all the subsets of the list S
(defun allsubsets (S)
    (if (null S)
        (list nil)
        (generate-subsets (car S) (allsubsets (cdr S)))
    )
)
; generate-subsets: returns a list containing the sublists of S plus the sublists of S
;                   when E is added to them
; test case: (generate-subsets 'b '(nil (a))) => (nil (b) (a) (b a))
(defun generate-subsets (E S)
    (if (null S)
        nil
        (cons (car S) (cons (cons E (car S)) (generate-subsets E (cdr S))))
    )
)

; QUESTION 6 ----------------------------------------------------------------------------
; substitute0: returns the list L with occurances of A replaced with E
(defun substitute0 (A E L)
    (cond
        ((null L) nil)
        ((atom (car L)) (cons (if (eq (car L) A) E (car L)) (substitute0 A E (cdr L))))
        (T (cons (substitute0 A E (car L)) (substitute0 A E (cdr L))))
    )
)

; QUESTION 7 reached --------------------------------------------------------------------
; reached: returns all the items in the list of pairs L that can be reached by traversing
;          from x not including x. A pair can be traversed if the first item in the pair
;          is known. Once the pair is traversed, the second item in the pair is known and
;          it can also be used to traverse pairs. At first, the only known item is x
; test case: (reached '1 '((1 2)(2 3)(3 4)(1 1)(2 5)(6 1)(1 1))) => (2 3 4 5)
(defun reached (x L)
    (list-reached (local-reached x L) (remove-reached x (cdr L)))
)
; list-reached: returns all the items in the list of pairs L that can be reached by
;               traversing from any of the items in S not including the items in S
(defun list-reached (S L)
    (cond
        ((null S) nil)
        (T (cons (car S) (append
            (list-reached (local-reached (car S) L) (remove-reached (car S) L))
            (list-reached (cdr S) (remove-reached (car S) L))
        )))
    )
)
; local-reached: returns all the items in the list of pairs L that can be reached by
;                traversing immediately (one step) from x not including x
(defun local-reached (x L)
    (cond
        ((null L) nil)
        ((and (eq (caar L) x) (not (eq (cadar L) x))) (cons (cadar L) (local-reached x (cdr L))))
        (T (local-reached x (cdr L)))
    )
)
; remove-reached: returns all the items in the list of pairs L that do not contain x
(defun remove-reached (x L)
    (cond
        ((null L) nil)
        ((or (eq (caar L) x) (eq (cadar L) x)) (remove-reached x (cdr L)))
        (T (cons (car L) (remove-reached x (cdr L))))
    )
)

; QUESTION 7 rank -----------------------------------------------------------------------
; rank: returns the items in S from most to least linked in the list of pairs L. An item
;       is linked in L if it is the second element in a pair where the same item is not
;       the first element
; test case: (rank '(1 2 3) '((1 2)(2 3)(1 3)(1 1)(1 1))) => (3 2 1)
(defun rank (S L)
    (remove-counts (sort-counts (get-counts S L)))
)
; get-counts: returns a list of the items in S and the number of times they were linked
;             in the list of pairs L
; test case: (get-counts '(1 2 3) '((1 2)(2 3)(1 3)(1 1)(1 1))) => ((1 0) (2 1) (3 2))
(defun get-counts (S L)
    (cond
        ((null S) nil)
        (T (cons (list (car S) (count-links (car S) L)) (get-counts (cdr S) L)))
    )
)
; count-links: returns the number of times x is linked in the list of pairs L
; test case: (count-links '1 '((1 2)(2 3)(1 3)(1 1)(1 1))) => 0
(defun count-links (x L)
    (cond
        ((null L) 0)
        ((eq (caar L) x) (+ 0 (count-links x (cdr L))))
        ((eq (cadar L) x) (+ 1 (count-links x (cdr L))))
        (T (+ 0 (count-links x (cdr L))))
    )
)
; remove-counts: returns a list with the first items of pairs in the list of pairs L
; test case: (remove-counts '((1 0)(2 1)(3 2)) => (1 2 3)
(defun remove-counts (L)
    (cond
        ((null L) nil)
        (T (cons (caar L) (remove-counts (cdr L))))
    )
)
; sort-counts: sorts the list of pairs L by the second element
(defun sort-counts (L)
    (sort L 'greater-than)
)
; greater-than: compares the list of pairs L by the second element
(defun greater-than (L1 L2)
    (> (cadr L1) (cadr L2))
)

