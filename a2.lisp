; Logan Vaughan
; 1577489
; CMPUT 325
; LEC B1
; Assignment 2

; fl-interp: Takes a program P and an expression E. Returns the result of evaluating E with respect to P.
; test case: (fl-interp '(+ 1 2) nil) => 3
; test case: (fl-interp '(f (f 2)) '( (f (X) = (* X X)) )) => 16
; test case: (fl-interp '(f (g 2) (g 1)) '( (f (X Y) = (+ X Y)) (g (X) = (+ 1 X)) )) => 5
(defun fl-interp (E P)
    (cond
        ((null E) nil)
        ((atom E) E)
        ((not (atom (car E))) (cons (fl-interp (car E) P) (fl-interp (cdr E) P)))

        ((and (equal (car E) 'if) (eq (fl-size (cdr E)) 3)) (if (fl-interp (cadr E) P) (fl-interp (caddr E) P) (fl-interp (cadddr E) P)))
        ((and (equal (car E) 'null) (eq (fl-size (cdr E)) 1)) (null (fl-interp (cadr E) P)))
        ((and (equal (car E) 'atom) (eq (fl-size (cdr E)) 1)) (atom (fl-interp (cadr E) P)))
        ((and (equal (car E) 'eq) (eq (fl-size (cdr E)) 2)) (eq (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'first) (eq (fl-size (cdr E)) 1)) (first (fl-interp (cadr E) P)))
        ((and (equal (car E) 'rest) (eq (fl-size (cdr E)) 1)) (rest (fl-interp (cadr E) P)))
        ((and (equal (car E) 'cons) (eq (fl-size (cdr E)) 2)) (cons (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'equal) (eq (fl-size (cdr E)) 2)) (equal (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'car) (eq (fl-size (cdr E)) 1)) (car (fl-interp (cadr E) P)))
        ((and (equal (car E) 'cdr) (eq (fl-size (cdr E)) 1)) (cdr (fl-interp (cadr E) P)))
        ((and (equal (car E) 'number) (eq (fl-size (cdr E)) 1)) (numberp (fl-interp (cadr E) P)))
        ((and (equal (car E) '+) (eq (fl-size (cdr E)) 2)) (+ (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) '-) (eq (fl-size (cdr E)) 2)) (- (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) '*) (eq (fl-size (cdr E)) 2)) (* (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) '>) (eq (fl-size (cdr E)) 2)) (> (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) '<) (eq (fl-size (cdr E)) 2)) (< (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) '=) (eq (fl-size (cdr E)) 2)) (= (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'and) (eq (fl-size (cdr E)) 2)) (and (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'or) (eq (fl-size (cdr E)) 2)) (or (fl-interp (cadr E) P) (fl-interp (caddr E) P)))
        ((and (equal (car E) 'not) (eq (fl-size (cdr E)) 2)) (not (fl-interp (cadr E) P)))

        ;((fl-find E P) (fl-interp (fl-replace (cadr (fl-find E P)) (cdr E) (cadddr (fl-find E P))) P)) ;normal order reduction
        ((fl-find E P) (fl-interp
            (fl-replace
                (cadr (fl-find E P))
                (mapcar (lambda (X) (funcall (lambda (Y) (fl-interp X Y)) P)) (cdr E))
                (cadddr (fl-find E P))
            )
            P
        )) ;applicative order reduction
        
        (T (mapcar (lambda (X) (funcall (lambda (Y) (fl-interp X Y)) P)) E)) ;when no function definition for (car E) in P, apply fl-interp to elements of E
    )
)

; fl-find: Takes a program P and an expression E. Returns the function definition in P that corresponds to the function being called in E.
; test case: (fl-find '(a 1 2) '( (a (X) = (+ X X)) (a (X Y) = (* X Y)) (a (X Y Z) = (* X (+ Y Z)) ) )) => (a (X Y) = (* X Y))
(defun fl-find (E P)
    (cond
        ((null P) nil)
        ((and (eq (caar P) (car E)) (eq (fl-size (cadar P)) (fl-size (cdr E)))) (car P))
        (T (fl-find E (cdr P)))
    )
)

; fl-find: Takes an expression E. Returns the number of items in E. Doesn't include nested items.
; test case: (fl-size '(1 (2 3) 4)) => 3
(defun fl-size (E)
    (cond
        ((null E) 0)
        (T (+ (fl-size (cdr E)) 1))
    )
)

; fl-replace: Returns the expression P with the occurances of the items in list A replaced with the respective items of list E.
; test case: (fl-replace '(X Y Z) '(1 2 3) '(+ X Y)) => (+ 1 2)
(defun fl-replace (A E P)
    (cond
        ((null A) P)
        ((null E) P)
        (T (fl-replace (cdr A) (cdr E) (fl-substitute (car A) (car E) P)))
    )
)

; fl-substitute: Returns the expression P with occurances of the item A replaced with the item E.
; test case: (fl-substitute 'X '1 '(+ X Y)) => (+ 1 Y)
(defun fl-substitute (A E P)
    (cond
        ((null P) nil)
        ((atom P) (car (fl-substitute A E (list P))))
        ((atom (car P)) (cons (if (eq (car P) A) E (car P)) (fl-substitute A E (cdr P))))
        (T (cons (fl-substitute A E (car P)) (fl-substitute A E (cdr P))))
    )
)

