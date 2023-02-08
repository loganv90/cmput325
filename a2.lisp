; Logan Vaughan
; 1577489
; CMPUT 325
; LEC B1
; Assignment 2


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
        
        (T (mapcar (lambda (X) (funcall (lambda (Y) (fl-interp X Y)) P)) E)) ;when no program for (car E) in P, apply fl-interp to elements of E
    )
)


; E -> (a 1 2)
; P -> ( (a (X) = (+ X X)) (a (X Y) = (* X Y)) (a (X Y Z) = (* X (+ Y Z)) )
;   -> (a (X Y) = (* X Y))
(defun fl-find (E P)
    (cond
        ((null P) nil)
        ((and (eq (caar P) (car E)) (eq (fl-size (cadar P)) (fl-size (cdr E)))) (car P))
        (T (fl-find E (cdr P)))
    )
)


; E -> (1 2 3)
;   -> 3
(defun fl-size (E)
    (cond
        ((null E) 0)
        (T (+ (fl-size (cdr E)) 1))
    )
)


; A -> (X Y Z)
; E -> (1 2 3)
; P -> (+ X Y)
;   -> (+ 1 2)
; fl-replace: returns the program P with occurances of the items in the list A replaced with the items in the list E
(defun fl-replace (A E P)
    (cond
        ((null A) P)
        ((null E) P)
        (T (fl-replace (cdr A) (cdr E) (fl-substitute (car A) (car E) P)))
    )
)


; A -> X
; E -> 1
; P -> (+ X Y)
;   -> (+ 1 Y)
; fl-substitute: returns the program P with occurances of A replaced with E
(defun fl-substitute (A E P)
    (cond
        ((null P) nil)
        ((atom P) (car (fl-substitute A E (list P))))
        ((atom (car P)) (cons (if (eq (car P) A) E (car P)) (fl-substitute A E (cdr P))))
        (T (cons (fl-substitute A E (car P)) (fl-substitute A E (cdr P))))
    )
)

