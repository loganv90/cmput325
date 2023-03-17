/*
Logan Vaughan
1577489
CMPUT 325
LEC B1
Assignment 3
*/


/* QUESTION 1 ----------------------------------------------------------------------------------- */

/*
setIntersect(+S1,+S2,-S3)
terms:
    +S1 : list of distinct atoms
    +S2 : list of atoms
    -S3 : list of atoms
description:
    Returns the list S3 that contains the atoms that exist in both S1 and S2.
test cases:
    setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q],S). % S = [a,b,c,e].
*/
setIntersect([], _, []).
setIntersect([X|Xt], Y, [X|Zt]) :-
    member_help(X, Y), !,
    setIntersect(Xt, Y, Zt).
setIntersect([X|Xt], Y, Z) :-
    \+ member_help(X, Y), !,
    setIntersect(Xt, Y, Z).

/*
member_help(+E,+L)
terms:
    +E : element
    +L : list
description:
    Returns true if E is in L.
test cases:
    member_help(1,[2,1]). % true.
*/
member_help(X, [X|_]).
member_help(X, [_|T]) :- member_help(X, T).


/* QUESTION 2 ----------------------------------------------------------------------------------- */

/*
swap(+L,-R)
terms:
    +L : list
    -R : list
description:
    Returns a list R with the elements of L but where every couple elements have been swapped.
test cases:
    swap([a,1,b,2], W). % W = [1,a,2,b].
    swap([a,1,b], W). % W = [1,a,b].
*/
swap([], []).
swap([X], [X]) :- !.
swap([X1,X2|Xt], [X2,X1|Yt]) :- swap(Xt, Yt).


/* QUESTION 3 ----------------------------------------------------------------------------------- */

/*
filter(+L,+OP,+N,-L1)
terms:
    +L  : nested list of numbers
    +OP : 'equal' or 'greaterThan' or 'lessThan'
    +N  : number
    -L1 : flat list of numbers
description:
    Returns a flat list L1 that contains the numbers from L that satisfy the condition OP relative
    to N.
test cases:
    filter([3,4,[5,2],[1,7,3]],greaterThan,3,W). % W = [4,5,7].
    filter([3,4,[5,2],[1,7,3]],equal,3,W). % W = [3,3].
    filter([3,4,[5,2],[1,7,3]],lessThan,3,W). % W = [2,1].
*/
filter([], _, _, []).
filter([X|Xt], OP, N, Y) :-
    \+ number(X), \+ is_list(X), !,
    filter(Xt, OP, N, Y).
filter([X|Xt], OP, N, [X|Yt]) :-
    number(X), compare_help(X, OP, N), !,
    filter(Xt, OP, N, Yt).
filter([X|Xt], OP, N, Y) :-
    number(X), \+ compare_help(X, OP, N), !,
    filter(Xt, OP, N, Y).
filter([X|Xt], OP, N, Y) :-
    is_list(X), !,
    filter(X, OP, N, Y1),
    filter(Xt, OP, N, Y2),
    append_help(Y1, Y2, Y).

/*
append_help(+L1,+L2,-L3)
terms:
    +L1 : list
    +L2 : list
    -L3 : list
description:
    Returns the list L3 which contains the elements of L1 followed by the elements of L2.
test cases:
    append_help([1],[2],L). % L = [1,2].
*/
append_help([],L,L).
append_help([A|L],L1,[A|L2]) :- append(L,L1,L2).

/*
compare_help(+X,+OP,+Y)
terms:
    +X  : number
    +OP : 'equal' or 'greaterThan' or 'lessThan'
    +Y  : number
description:
    Returns true if X satisfies the condition OP relative to Y.
test cases:
    compare_help(4,greaterThan,3). % true.
    compare_help(3,equal,3). % true.
    compare_help(2,lessThan,3). % true.
*/
compare_help(X, 'greaterThan', Y) :- X > Y.
compare_help(X, 'lessThan', Y) :- X < Y.
compare_help(X, 'equal', Y) :- X = Y.


/* QUESTION 4 ----------------------------------------------------------------------------------- */

/*
countAll(+L1,-L2)
terms:
    +L1 : list of atoms
    -L2 : sorted list of elements in the form [<atom>, <number>]
description:
    Returns a list of elements in the form [<atom>, <number>] with each <atom> being an atom in L1
    and each <number> being the number of times the atom appears in L1 where the elements are
    sorted in increasing order by <number> and then by <atom>.
test cases:
    countAll([a,b,e,c,c,b],N). % N = [[a,1],[e,1],[b,2],[c 2]].
*/
countAll(X, Y) :-
    countAll_count(X, Z),
    countAll_sort(Z, Y), !.

/*
countAll_count(+L1,-L2)
terms:
    +L1 : list of atoms
    -L2 : list of elements in the form [<atom>, <number>]
description:
    Returns a list of elements in the form [<atom>, <number>] with each <atom> being an atom in L1
    and each <number> being the number of times the atom appears in L1.
test cases:
    countAll_count([a,b,e,c,c,b],N). % N = [[a,1],[b,2],[e,1],[c,2]].
*/
countAll_count([], []).
countAll_count([X|Xt], [[X, Yv]|Yt]) :-
    countAll_find(X, Xt, Yv, Z),
    countAll_count(Z, Yt).

/*
countAll_find(+E,+L1,-C,-L2)
terms:
    +E  : element
    +L1 : list
    -C  : number
    -L2 : list
description:
    Returns the number C being one plus the number of occurances of E in L1 and the list L2 being
    the elements in L1 but without the occurances of E.
test cases:
    countAll_find(a,[a,b,a,c],C,L2). % C = 3, L2 = [b,c].
*/
countAll_find(_, [], 1, []) :- !.
countAll_find(X, [X|Yt], N, Z) :-
    countAll_find(X, Yt, M, Z), !,
    N is M + 1.
countAll_find(X, [Y|Yt], N, [Y|Zt]) :-
    \+ X = Y,
    countAll_find(X, Yt, N, Zt), !.

/*
countAll_compare(+OP,+S1,+S2)
terms:
    +OP : < or >
    +S1 : elements in the form [<atom>, <number>]
    +S2 : elements in the form [<atom>, <number>]
description:
    Returns true if the S1 is bigger than S2. Compares first by <number> and then by <atom>.
test cases:
    countAll_compare(<,[a,2],[b,3]). % true.
    countAll_compare(>,[a,3],[b,2]). % true.
*/
countAll_compare(<, [_, X], [_, Y]) :- X < Y.
countAll_compare(>, [_, X], [_, Y]) :- X > Y.
countAll_compare(<, [X, _], [Y, _]) :- X @< Y.
countAll_compare(>, [X, _], [Y, _]) :- X @>= Y.

/*
countAll_sort(+L, -S)
terms:
    +L : list of elements in the form [<atom>, <number>]
    -S : list of elements in the form [<atom>, <number>]
description:
    Returns a list containing the elements in L sorted in increasing order by <number> first and
    then by <atom>.
test cases:
    countAll_sort([[a,2],[b,1]],S). % S = [[b,1],[a,2]].
*/
countAll_sort(List, Sorted) :- predsort(countAll_compare, List, Sorted).


/* QUESTION 5 ----------------------------------------------------------------------------------- */

/*
sub(+L,+S,-L2)
terms:
    +L  : list
    +S  : list of elements in the form [<element1>, <element2>]
    -L1 : list
description:
    Finds all the instances in L of every <element1> from S and relpaces then with the associated
    <element2> from S. Then, returns the list with the replacements as L1.
test cases:
    sub([a,[a,d],[e,a]],[[a,2]],L). % L = [2,[2,d],[e,2]].
*/
sub([], _, []).
sub([X|Xt], Y, [Z|Zt]) :-
    is_list(X), !,
    sub(X, Y, Z),
    sub(Xt, Y, Zt).
sub([X|Xt], Y, [X|Zt]) :-
    \+ sub_find(X, Y, _), !,
    sub(Xt, Y, Zt).
sub([X|Xt], Y, [Z|Zt]) :-
    sub_find(X, Y, Z), !,
    sub(Xt, Y, Zt).

/*
sub_find(+E1,+L,-E2)
terms:
    +E1 : element
    +L  : list of elements in the form [<element1>, <element2>]
    -E2 : element
description:
    Finds the element in L where <element1> is equal to E1. Then, returns the associated
    <element2> as E2.
test cases:
    sub_find(a,[[b,1],[a,2]],E). % E = 2.
*/
sub_find(X, [[X, Y]|_], Y).
sub_find(X, [[_, _]|Y], Z) :- sub_find(X, Y, Z).


/* QUESTION 6 ----------------------------------------------------------------------------------- */

/*
clique(+L)
terms:
    +L1 : list
description:
    Returns a list with all the cliques depending on the existing edges and nodes.
test cases (with the following present: edge(a,b). edge(b,c). edge(c,a). node(a). node(b). node(c).):
    clique(L). % L = [a,b,c] ; L = [b,c] ; L = [a,c] ; L = [c] ; L = [a,b] ; L = [b] ; L = [a] ; L = [].
    clique([a, b]). % true.
    clique([f]). % false.
*/
clique(X) :-
    nodes(N),
    generate_subsets(N, S),
    generate_cliques(S, C),
    member_help(X, C).

/*
nodes(+L)
terms:
    +L1 : list
description:
    Returns a list with all the existing nodes.
test cases (with the following present: edge(a,b). edge(b,c). edge(c,a). node(a). node(b). node(c).):
    nodes(X). % X = [a,b,c].
*/
nodes(X) :- findall(Y, node(Y), X).

/*
generate_cliques(+L1,-L2)
terms:
    +L1 : list of lists
    -L2 : list of lists
description:
    Returns the list L2 containing the lists from L1 that are cliques depending on existing edges.
test cases (with the following present: edge(a,b). edge(b,c). edge(c,a). node(a). node(b). node(c).):
    generate_cliques([[a,b,c],[a,d],[]],L2). % L2 = [[a,b,c],[]].
*/
generate_cliques([], []).
generate_cliques([Subset|Subsets], [Subset|Cliques]) :-
    check_clique(Subset),
    generate_cliques(Subsets, Cliques).
generate_cliques([Subset|Subsets], Cliques) :-
    \+ check_clique(Subset),
    generate_cliques(Subsets, Cliques).

/*
check_clique(+L)
terms:
    -L : list
description:
    Returns true if there are edges between each element in L and all the other element in L.
test cases (with the following present: edge(a,b). edge(b,c). edge(c,a). node(a). node(b). node(c).):
    check_clique([a,b,c]). % true.
*/
check_clique([]).
check_clique([X|Xt]) :-
    check_edges(X, Xt),
    check_clique(Xt).

/*
check_edges(+E,-L)
terms:
    +E : element
    -L : list
description:
    Returns true if there are edges between E and every element in L.
test cases (with the following present: edge(a,b). edge(b,c). edge(c,a). node(a). node(b). node(c).):
    check_edges(a, [b,c]). % true.
*/
check_edges(_, []).
check_edges(X, [Y|Yt]) :-
    (edge(X,Y);edge(Y,X)),
    check_edges(X, Yt).

/*
generate_subsets(+L1,-L2)
terms:
    +L1 : list
    -L2 : list of lists
description:
    Returns the list L2 containing all the subsets of the elements in the list L1.
test cases:
    generate_subsets([a,b,c],L2). % L2 = [[a,b,c],[b,c],[a,c],[c],[a,b],[b],[a],[]].
*/
generate_subsets([], [[]]).
generate_subsets([X|Xt], Y) :-
    generate_subsets(Xt, Z),
    adding_item(X, Z, Y).

/*
adding_item(+E,+L1,-L2)
terms:
    +E  : element
    +L1 : list of lists
    -L2 : list of lists
description:
    Returns the list L2 containing all the lists of L1 and all the lists of L1 including E.
test cases:
    adding_item(a,[[b],[]],L2). % L2 = [[a,b],[b],[a],[]].
*/
adding_item(_, [], []).
adding_item(X, [Y|Yt], [[X|Y],Y|Zt]) :- adding_item(X, Yt, Zt).


/* QUESTION 7 ----------------------------------------------------------------------------------- */
:- use_module(library(lists)).

/*
convert(+Term,-Result)
terms:
    +Term   : list of atoms
    -Result : list of atoms
description:
    Returns the list Result composed of the atoms from Term, but with the atoms between two 'q's
    being processed by outside(+L1,-L2).
test cases:
    convert([e,e,a,e,b,e],R). % R = [w,w].
    convert([e,q,a,b,e,e],R). % R = [q,w,w].
    convert([e,a,e,e],R). % R = [w].
    convert([e,q,a,e,b,q,e,a,e],R). % R = [q,a,e,b,q,w].
    convert([a,q,e,l,q,r,e,q,b,e],R). % R = [w,q,e,l,q,w,q,w].
    convert([q,e,q,b,q,e,l,q,a,e],R). % R = [q,e,q,w,q,e,l,q,w].
*/
convert([], []).
convert(In, Out) :-
    append(Beforeq, ['q'|Afterq], In), !,
    outside(Beforeq, Out1),
    convert_in(Afterq, Out2),
    append(Out1, ['q'|Out2], Out).
convert(In, Out) :-
    \+ append(_, ['q'|_], In), !,
    outside(In, Out).

/*
convert_in(+Term,-Result)
terms:
    +Term   : list of atoms
    -Result : list of atoms
description:
    Tries to find a 'q' in Term. If success, preserves the atoms until that q and calls
    convert(+Term,-Result) on the remaining atoms. If failure, processes the atoms with
    outside(+L1,-L2).
test cases:
    convert_in([a,b,q,d,e,f],L2). % L2 = [a,b,q,w,w].
*/
convert_in([], []).
convert_in(In, Out) :-
    append(Beforeq, ['q'|Afterq], In), !,
    convert(Afterq, Out2),
    append(Beforeq, ['q'|Out2], Out).
convert_in(In, Out) :-
    \+ append(_, ['q'|_], In), !,
    outside(In, Out).

/*
outside(+L1,-L2)
terms:
    +L1 : list of atoms
    -L2 : list of atoms
description:
    Returns the list L2 containing the atoms from L1, but without 'e's, keeping 'q's, and with the
    other atoms changed into 'w's.
test cases:
    outside([a,b,q,d,e,f],L). % L = [w,w,q,w,w].
*/
outside([], []).
outside(['e'|Xt], Yt) :- !, outside(Xt, Yt).
outside(['q'|Xt], ['q'|Yt]) :- !, outside(Xt, Yt).
outside([_|Xt], ['w'|Yt]) :- !, outside(Xt, Yt).

