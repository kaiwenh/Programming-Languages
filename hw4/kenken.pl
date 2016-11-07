

%test case from spec
kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).

% --------------------------------------------
% Imlementation for kenken (using finite domain)
% --------------------------------------------

% Transpose a matrix
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


% hasLenN/2 - check if list L has length N
hasLenN(N, L):- length(L, N).

% get the number at index I-J and store into Val
getNum(I-J, Matrix, Val):- nth(I, Matrix, Row), 
                          nth(J, Row, Val).

%check if every element of L is in the range [1,N]
checkRange(N, L):- fd_domain(L, 1, N).


% Checking Constraints
checkConstraints(T, C) :- check(T, C).
check(T, +(Result, List)) :- sum(T, Result, List, 0).
check(T, -(Result, A, B)) :- subtract(T, Result, A, B).
check(T, *(Result, List)) :- mult(T, Result, List, 1).
check(T, /(Result, A, B)) :- divide(T, Result, A, B).

% Sum:
sum(_, Result, [], Result).
sum(T, Result, [Head|Tail], Acc) :- 
    getNum(Head, T, Num), 
    Cur #= Acc + Num, 
    sum(T, Result, Tail, Cur). 

% Product:
mult(_, Result, [], Result).
mult(T, Result, [Head|Tail], Acc) :-
    getNum(Head, T, Num),
    Cur #= Acc * Num,
    mult(T, Result, Tail, Cur).

% Subtraction:
subtract(_, Result, _, _, Result).
subtract(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc #=  X - Y,
    subtract(T, Result, A, B, Acc).
subtract(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc #=  Y - X,
    subtract(T, Result, A, B, Acc).

% Division:
divide(_, Result, _, _, Result).
divide(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc #= X / Y,
    divide(T, Result, A, B, Acc).
divide(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc #= Y / X,
    divide(T, Result, A, B, Acc).



%kenken solver
kenken(N, C, T) :-
    length(T, N),               %check T has N rows
    maplist(hasLenN(N), T),     %check T has N columns
    maplist(checkRange(N), T),  %check if every row has unique numbers and all within the domain [1,N]
    maplist(fd_all_different, T),   %check if every row has unique numbers
    transpose(T, T_trans),      %transpose T
    maplist(fd_all_different, T_trans),   %check if every column has unique numbers
    maplist(checkConstraints(T), C),      % check every constraint in C is satisfied by T
    maplist(fd_labeling, T).              %get the values for matches
    %statistics.               %for collecting runtime and memory use
    

% --------------------------------------------
% Implementation for plain_kenken (without using finite domain)
% --------------------------------------------

%build the domain L = [1,N] 
getRange(N, L) :- findall(Num, between(1, N, Num), L).  %put numbers 1 through N into L

%check if a list has all unique numbers
checkUnique([]).
checkUnique([Head|Tail]) :- \+(member(Head, Tail)), checkUnique(Tail).



% Checking Constraints (plain version)
checkConstraints_plain(T, C) :- plain_check(T, C).
plain_check(T, +(Result, List)) :- plain_sum(T, Result, List, 0).
plain_check(T, -(Result, A, B)) :- plain_subtract(T, Result, A, B).
plain_check(T, *(Result, List)) :- plain_mult(T, Result, List, 1).
plain_check(T, /(Result, A, B)) :- plain_divide(T, Result, A, B).

% Sum:
plain_sum(_, Result, [], Result).
plain_sum(T, Result, [Head|Tail], Acc) :- 
    getNum(Head, T, Num), 
    Cur is Acc + Num, 
    plain_sum(T, Result, Tail, Cur). 

% Product:
plain_mult(_, Result, [], Result).
plain_mult(T, Result, [Head|Tail], Acc) :-
    getNum(Head, T, Num),
    Cur is Acc * Num,
    plain_mult(T, Result, Tail, Cur).

% Subtraction
plain_subtract(_, Result, _, _, Result).
plain_subtract(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc is  X - Y,
    subtract(T, Result, A, B, Acc).
plain_subtract(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc is  Y - X,
    plain_subtract(T, Result, A, B, Acc).

% Division
plain_divide(_, Result, _, _, Result).
plain_divide(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc is X / Y,
    plain_divide(T, Result, A, B, Acc).
plain_divide(T, Result, A, B) :-
    getNum(A, T, X),
    getNum(B, T, Y),
    Acc is Y / X,
    plain_divide(T, Result, A, B, Acc).


%plain_kenken solver
plain_kenken(N, C, T):- length(T, N),               %T has N rows
                        maplist(hasLenN(N), T),     % T has N columns
                        getRange(N, L),           % get the list of valid numbers (domain) as L
                        maplist(permutation(L), T),  %check if every row has unique numbers and all within the domain [1,N]
                        transpose(T, T_trans),       %transpose matrix
                        maplist(checkUnique, T_trans), %check if every column has unique numbers (no need to check range again)
                        maplist(checkConstraints_plain(T), C).   % check every constraint in C is satisfied by T
                        %statistics.               %for collecting runtime and memory use

