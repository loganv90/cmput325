#const steps=17.

car(car1).
car(car2).
car(car3).
car(car4).

location(a).
location(b).

at(car1,a,0).
at(car2,a,0).
at(car3,a,0).
at(car4,a,0).

at(ferry,b,0).

goal(T):-
at(car1,b,T),
at(car2,b,T),
at(car3,b,T),
at(car4,b,T),
at(ferry,a,T).
