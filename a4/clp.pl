/*
Logan Vaughan
1577489
CMPUT 325
LEC B1
Assignment 4
*/


insert_data :-
    assert(c325(fall_2021,aperf,15,15,15,15,75,99)),
    assert(c325(fall_2021,john,14,13,15,10,76,87)),
    assert(c325(fall_2021,lily, 9,12,14,14,76,92)),
    assert(c325(fall_2021,peter,8,13,12,9,56,58)),
    assert(c325(fall_2021,ann,14,15,15,14,76,95)),
    assert(c325(fall_2021,ken,11,12,13,14,54,87)),
    assert(c325(fall_2021,kris,13,10,9,7,60,80)),
    assert(c325(fall_2021,audrey,10,13,15,11,70,80)),
    assert(c325(fall_2021,randy,14,13,11,9,67,76)),
    assert(c325(fall_2021,david,15,15,11,12,66,76)),
    assert(c325(fall_2021,sam,10,13,10,15,65,67)),
    assert(c325(fall_2021,kim,14,13,12,11,68,78)),
    assert(c325(fall_2021,perf,15,15,15,15,80,100)),
    assert(setup(fall_2021,as1,15,0.1)),
    assert(setup(fall_2021,as2,15,0.1)),
    assert(setup(fall_2021,as3,15,0.1)),
    assert(setup(fall_2021,as4,15,0.1)),
    assert(setup(fall_2021,midterm,80,0.2)),
    assert(setup(fall_2021,final,100,0.4)).


/* QUESTION 1 ----------------------------------------------------------------------------------- */

/*
query1(+Semester,+Name,-Total)
terms:
    +Semester : constant
    +Name     : constant
    -Total    : list of atoms
description:
    Returns the total grade of the student from the semester Semester with the name Name.
test cases:
    query1(fall_2021, kim, X). % X = 81.53333333333335.
*/
query1(Semester, Name, Total) :-
    c325(Semester, Name, A1, A2, A3, A4, M, F), !,
    setup(Semester, as1, A1_total, A1_weight), !,
    setup(Semester, as2, A2_total, A2_weight), !,
    setup(Semester, as3, A3_total, A3_weight), !,
    setup(Semester, as4, A4_total, A4_weight), !,
    setup(Semester, midterm, M_total, M_weight), !,
    setup(Semester, final, F_total, F_weight), !,
    Total is A1_weight*A1/A1_total + A2_weight*A2/A2_total + A3_weight*A3/A3_total + A4_weight*A4/A4_total + M_weight*M/M_total + F_weight*F/F_total.

/*
query2(+Semester,-L)
terms:
    +Semester : constant
    -L        : list of atoms
description:
    Returns a list L of the names of the students who scored better on the final than the midterm.
test cases:
    query2(fall_2021, X). % X = [aperf,ken,kris].
*/
query2(Semester, L) :-
    setup(Semester, midterm, M_total, _), !,
    setup(Semester, final, F_total, _), !,
    findall(Name, (c325(Semester, Name, _, _, _, _, M, F), F/F_total > M/M_total), L).

/*
query3(+Semester,+Name,+Type,+NewMark)
terms:
    +Semester : constant
    +Name     : constant
    +Type     : constant
    +NewMark  : number
description:
    Changes the mark of the assignment Type, of the student Name, of the semester Semester, to NewMark.
test cases:
    query3(fall_2021, kim, final, 87). % true.
    query3(fall_2014, jim, as1, 53). % record not found true.
*/
query3(Semester, Name, final, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, A1, A2, A3, A4, M, _) ),
    assert( c325(Semester, Name, A1, A2, A3, A4, M, NewMark) ).
query3(Semester, Name, midterm, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, A1, A2, A3, A4, _, F) ),
    assert( c325(Semester, Name, A1, A2, A3, A4, NewMark, F) ).
query3(Semester, Name, as4, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, A1, A2, A3, _, M, F) ),
    assert( c325(Semester, Name, A1, A2, A3, NewMark, M, F) ).
query3(Semester, Name, as3, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, A1, A2, _, A4, M, F) ),
    assert( c325(Semester, Name, A1, A2, NewMark, A4, M, F) ).
query3(Semester, Name, as2, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, A1, _, A3, A4, M, F) ),
    assert( c325(Semester, Name, A1, NewMark, A3, A4, M, F) ).
query3(Semester, Name, as1, NewMark) :-
    c325(Semester, Name, _, _, _, _, _, _), !,
    retract( c325(Semester, Name, _, A2, A3, A4, M, F) ),
    assert( c325(Semester, Name, NewMark, A2, A3, A4, M, F) ).
query3(Semester, Name, _, _) :-
    \+ c325(Semester, Name, _, _, _, _, _, _), !,
    write('record not found').


/* QUESTION 2 ----------------------------------------------------------------------------------- */

:- use_module(library(clpfd)).

/*
encrypt(+W1,+W2,+W3)
terms:
    +W1 : list of variables
    +W2 : list of variables
    +W3 : list of variables
description:
    Each input list is treated as a number. Outputs the numbers 0..9 that variables can take on when W1 + W2 = W3.
test cases:
    encrypt([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]). % S=9,E=5,N=6,D=7,M=1,O=0,R=8,Y=2 ; false.
*/
encrypt(W1,W2,W3) :-
    length(W1,N),
    length(W3,N1),
    append(W1,W2,W),
    append(W,W3,L),
    list_to_set(L,Letters),
    [LeadLetter1|_] = W1,
    [LeadLetter2|_] = W2,
    [LeadLetter3|_] = W3,
    !,
    Letters ins 0..9,
    LeadLetter1 #\= 0,
    LeadLetter2 #\= 0,
    LeadLetter3 #\= 0,
    all_different(Letters),
    encrypt_product(W1, N, X1),
    encrypt_product(W2, N, X2),
    encrypt_product(W3, N1, X3),
    X1 + X2 #= X3,
    label(Letters).

/*
encrypt_product(+Word,+Length,+Total)
terms:
    +Word   : list of variables
    +Length : number
    +Total  : number
description:
    The numerical value of the list of variables Word with length Length is outputted as Total.
test cases:
    encrypt_product([1,2,3], 3, X). % X = 123.
*/
encrypt_product([], _, 0).
encrypt_product([X|Xt], L, T) :-
    L1 is L - 1,
    E is 10 ** L1,
    encrypt_product(Xt, L1, T1),
    T #= T1 + X * E.


/* QUESTION 3 ----------------------------------------------------------------------------------- */

/* 
The goal of Sudoku is to fill in a 9 by 9 grid with digits 
so that each column, row, and 3 by 3 section contain the 
numbers between 1 to 9. At the beginning of the game, 
the 9 by 9 grid will have some of the squares filled in. 
Your job is to fill in the missing digits and complete the grid. 
*/

:- use_module(library(clpfd)).

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall-distinct(Rows),
        % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall-distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
	 [_,2,_,_,_,_,4,5,6],
	 [_,_,3,2,_,5,_,_,_],
	 [_,_,_,4,_,_,8,_,5],
	 [7,8,9,_,5,_,_,_,_],
	 [_,_,_,_,_,6,2,_,3],
	 [8,_,1,_,_,_,7,_,_],
	 [_,_,_,1,2,3,_,8,_],
	 [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

/*
Example:
?- t(Rows).
[1,5,6,8,9,4,3,2,7]
[9,2,8,7,3,1,4,5,6]
[4,7,3,2,6,5,9,1,8]
[3,6,2,4,1,7,8,9,5]
[7,8,9,3,5,2,6,4,1]
[5,1,4,9,8,6,2,7,3]
[8,3,1,5,4,9,7,6,2]
[6,9,7,1,2,3,5,8,4]
[2,4,5,6,7,8,1,3,9]
Rows = [[1, 5, 6, 8, 9, 4, 3, 2|...], [9, 2, 8, 7, 3, 1, 4|...], [4, 7, 3, 2, 6, 5|...], [3, 6, 2, 4, 1|...], [7, 8, 9, 3|...], [5, 1, 4|...], [8, 3|...], [6|...], [...|...]].
*/

/*
grid(+X,+Rows)
terms:
    +X    : number
    -Rows : list of lists
description:
    Returns a grid of size X by X in the variable Rows.
test cases:
    grid(2, X). % X = [[_, _], [_, _]].
*/
grid(X, Rows) :-
    length(Rows, X),
    maplist(same_length(Rows), Rows).

/*
xtranspose(+Rows,+Columns)
terms:
    +Rows    : list of lists
    -Columns : list of lists
description:
    Returns the transposed matrix Rows in the variable Columns.
test cases:
    xtranspose([[1,2],[5,6]], X) % X = [[1, 5], [2, 6]].
*/
xtranspose([], []).
xtranspose([Row|Rows], Columns) :-
    length(Row, L),
    makeListOfLists(L, E),
    makeTranspose([Row|Rows], E, Columns).

/*
makeTranspose(+Rows,+Y1,-Y3)
terms:
    +Rows : list of lists
    +Y1   : list of lists
    -Y3   : list of lists
description:
    Appends the transposed matrix Rows to the front of Y1 and returns it in Y3.
test cases:
    makeTranspose([[1,2],[5,6]], [[9],[0]], X). % X = [[1, 5, 9], [2, 6, 0]].
*/
makeTranspose([], Y, Y).
makeTranspose([Row|Rows], Y1, Y3) :-
    makeTranspose(Rows, Y1, Y2),
    appendListOfLists(Row, Y2, Y3).

/*
makeListOfLists(+L,-Rows)
terms:
    +L    : number
    -Rows : list of lists
description:
    Makes a list of length L containing empty lists and returns it in Rows.
test cases:
    makeListOfLists(2, X). % X = [[], []].
*/
makeListOfLists(0, []) :- !.
makeListOfLists(X, [[]|Zt]) :-
    X1 is X - 1,
    makeListOfLists(X1, Zt).

/*
appendListOfLists(+Row,+In,-Out)
terms:
    +Row : list
    +In  : list of lists
    +Out : list of lists
description:
    Appends the transposed array Row to the front of In and returns it in Out.
test cases:
    appendListOfLists([1,2], [[5],[6]], X). % X = [[1, 5], [2, 6]].
*/
appendListOfLists([], [], []).
appendListOfLists([X|Xt], [Y|Yt], [[X|Y]|Zt]) :-
    appendListOfLists(Xt, Yt, Zt).
    
/*
xall-distinct(+Rows)
terms:
    +Rows : list of lists
description:
    Returns true if the elements in each row of the matrix Rows are distinct.
test cases:
    xall-distinct([[1,1],[5,6]]). % false.
*/
xall-distinct([]).
xall-distinct([Row|Rows]) :-
    makeRowDistinct(Row),
    xall-distinct(Rows).

/*
makeRowDistinct(+Rows)
terms:
    +Row : list
description:
    Returns true if the elements in Row are distinct.
test cases:
    makeRowDistinct([1,1]). % false.
*/
makeRowDistinct([]).
makeRowDistinct([X|Xt]) :-
    makeListDistinct(X, Xt),
    makeRowDistinct(Xt).

/*
makeListDistinct(+Rows)
terms:
    +E   : number
    +Row : list
description:
    Returns true E does not exist in Row.
test cases:
    makeListDistinct(1, [1,2]) % false.
*/
makeListDistinct(_, []).
makeListDistinct(Y, [X|Xt]) :-
    Y #\= X,
    makeListDistinct(Y, Xt).


/* QUESTION 4 ----------------------------------------------------------------------------------- */

:- use_module(library(clpfd)).

paper(1,lily,xxx,ai).
paper(2,peter,john,database).
paper(3,ann,xxx,theory).
paper(4,ken,lily,network).
paper(5,kris,xxx,games).

reviewer(lily,theory,network).
reviewer(john,ai,theory).
reviewer(peter,database,network).
reviewer(ann,theory,network).
reviewer(kris,theory,games).
reviewer(ken,database,games).
reviewer(bill,database,ai).
reviewer(jim,theory,games).

workLoadAtMost(2).

/*
assign(+W1,+W2)
terms:
    +W1 : list
    +W2 : list
description:
    Returns W1 and W2 which denote the reviewers that can review each paper.
test cases:
    assign(W1, W2). % W1 = [john, ken, lily, peter, ken], W2 = [bill, bill, john, ann, jim] ...
*/
assign(N1, N2) :-
    findall(Id, paper(Id, _, _, _), Papers),
    findall(Name, reviewer(Name, _, _), Reviewers),
    workLoadAtMost(Max),
    length(Papers, PaperCount),
    length(Reviewers, ReviewerCount),
    length(W1, PaperCount),
    length(W2, PaperCount),
    findall(X, between(1,ReviewerCount,X), ReviewerNumbers),
    !,
    W1 ins 1..ReviewerCount,
    W2 ins 1..ReviewerCount,
    assign_own_paper(W1, Papers, Reviewers),
    assign_own_paper(W2, Papers, Reviewers),
    assign_paper_count(Max, W1, W2, ReviewerNumbers),
    assign_different_reviewers(W1, W2),
    label(W1),
    label(W2),
    names(W1, Reviewers, N1),
    names(W2, Reviewers, N2).

/*
names(+W,+Reviewers,-Names)
terms:
    +W         : list
    +Reviewers : list
    +Names     : list
description:
    W is a list of indexes. Returns Names, a list of values, containing the values at the indexes in Reviewers. Note: one indexed.
test cases:
    names([1], [name], X). % X = [name].
*/
names([], _, []).
names([W|Wt], Reviewers, [Name|Names]) :-
    nth1(W, Reviewers, Name),
    names(Wt, Reviewers, Names).

/*
count(+E,+L,-C)
terms:
    E : number
    L : list
    C : number
description:
    Returns the number C of items E in the list L.
test cases:
    count(1, [1], X). % X = 1 ; false.
*/
count(_, [], 0).
count(X, [X|Wt], C) :-
    count(X, Wt, C1),
    C is C1 + 1.
count(X, [W|Wt], C) :-
    dif(X, W),
    count(X, Wt, C).

/*
assign_own_paper(+W,+Papers,+Reviewers)
terms:
    W         : list
    Papers    : list
    Reviewers : list
description:
    Constrains W so that reviewers can't review own paper, and so that reviewers can't review papers that aren't about their subject.
*/
assign_own_paper([], [], _).
assign_own_paper([W|Wt], [Paper|Papers], Reviewers) :-
    paper(Paper, Name1, xxx, _),
    nth1(R1, Reviewers, Name1),
    !,
    W #\= R1,
    assign_subject_reviewer(W, Paper, Reviewers, Reviewers),
    assign_own_paper(Wt, Papers, Reviewers).
assign_own_paper([W|Wt], [Paper|Papers], Reviewers) :-
    paper(Paper, Name1, Name2, _),
    nth1(R1, Reviewers, Name1),
    nth1(R2, Reviewers, Name2),
    !,
    W #\= R1,
    W #\= R2,
    assign_subject_reviewer(W, Paper, Reviewers, Reviewers),
    assign_own_paper(Wt, Papers, Reviewers).

/*
assign_subject_reviewer(+W,+Papers,+Reviewers,+AllReviewers)
terms:
    W            : list
    Papers       : list
    Reviewers    : list
    AllReviewers : list
description:
    Constrains W so that reviewers can't review papers that aren't about their subject.
*/
assign_subject_reviewer(_, _, [], _).
assign_subject_reviewer(W, Paper, [Reviewer|Reviewers], ReviewersAll) :-
    paper(Paper, _, _, S),
    reviewer(Reviewer, S1, S2),
    nth1(R, ReviewersAll, Reviewer),
    \+ S == S1,
    \+ S == S2,
    !,
    W #\= R,
    assign_subject_reviewer(W, Paper, Reviewers, ReviewersAll).
assign_subject_reviewer(W, Paper, [_|Reviewers], ReviewersAll) :-
    assign_subject_reviewer(W, Paper, Reviewers, ReviewersAll).

/*
assign_paper_count(+Max,+W1,+W2,+ReviewerIndexes)
terms:
    Max             : list
    W1              : list
    W2              : list
    ReviewerIndexes : list
description:
    Constrains W1 and W2 so that reviewers can't review more than the max number of papers.
*/
assign_paper_count(_, _, _, []).
assign_paper_count(Max, W1, W2, [ReviewerNumber|ReviewerNumbers]) :-
    count(ReviewerNumber, W1, C1),
    count(ReviewerNumber, W2, C2),
    C1 + C2 #=< Max,
    assign_paper_count(Max, W1, W2, ReviewerNumbers).

/*
assign_different_reviewers(+W1,+W2)
terms:
    W1              : list
    W2              : list
description:
    Constrains W1 and W2 so that the same reviewer can't review the same paper twice.
*/
assign_different_reviewers([], []).
assign_different_reviewers([W1|W1t], [W2|W2t]) :-
    W1 #\= W2,
    assign_different_reviewers(W1t, W2t).
