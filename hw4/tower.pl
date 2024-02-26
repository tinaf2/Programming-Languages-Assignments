% ntower:

% how to transpose a matrix, code taken from: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog*/
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

none_bigger([],_).
none_bigger([H|Rest], X) :-
            H #< X,
            none_bigger(Rest, X).

check_row([], _, X) :- X = 0.
check_row([X|Rest], Oldlist, Count) :-
            append(Oldlist, [X], Newlist),
            check_row(Rest, Newlist, Count1),
            (none_bigger(Oldlist, X) -> Count is Count1 + 1; Count1 = Count).

% check one list of values, assuming it is the left side of the board
check_lists([], []).
check_lists([Row|Rest], [Count|RestCounts]) :-
            check_row(Row, [], Count2), Count2 = Count,
            check_lists(Rest, RestCounts).

reverse_lists([], []).
reverse_lists([X|Rest], [XRev|RestRev]) :-
            reverse(X, XRev),
            reverse_lists(Rest, RestRev).

% check to make sure visibility requirements are met from every angle
visibility(T, Transposed_T, Top, Bottom, Left, Right) :-
            check_lists(T, Left),
            reverse_lists(T, TRev), check_lists(TRev, Right),
            check_lists(Transposed_T, Top),
            reverse_lists(Transposed_T, Rev_transposed_T), check_lists(Rev_transposed_T, Bottom).


set_constraints(_, []).
set_constraints(N, [H|T]) :-
            length(H, N),
            fd_domain(H, 1, N),
            fd_all_different(H),
            set_constraints(N, T).


ntower(N, T, C) :-
	        C = counts(Top, Bottom, Left, Right),
            length(Top, N),
            length(Bottom, N),
            length(Left, N),
            length(Right, N),
	          length(T, N),
            set_constraints(N, T),
            transpose(T, T_tr),
            set_constraints(N, T_tr),
            maplist(fd_labeling, T),
            visibility(T, T_tr, Top, Bottom, Left, Right).



% plain_ntower:

check_domain([], _, _).
check_domain([X | R], Lower, Upper) :-
            X #> Lower - 1, X #< Upper + 1,
            check_domain(R, Lower, Upper).

plain_domain(N, X) :-
	        check_domain(X, 1, N).

check_diff(_, []).
check_diff(X, [H | Rest]) :-
            X #\= H,
            check_diff(X, Rest).

plain_all_unwrap(_, []).
plain_all_unwrap(Oldlist, [X | Rest]) :-
            check_diff(X, Rest),
            append(Oldlist, [X], Newlist),
            plain_all_unwrap(Newlist, Rest).

check_all_different(L) :-
	        plain_all_unwrap([], L). 

plain_labeling(N, L) :-
            findall(Num, between(1, N, Num), X), 
            permutation(X, L).

plain_set_constraints(_, []).
plain_set_constraints(N, [H|T]) :-
            length(H, N),
            plain_domain(N, H),
            check_all_different(H),
            plain_set_constraints(N, T).

plain_ntower(N, T, C) :-
	        C = counts(Top, Bottom, Left, Right),
            length(Top, N),
            length(Bottom, N),
            length(Left, N),
            length(Right, N),
	          length(T, N),
            plain_set_constraints(N, T),
            transpose(T, F),
            plain_set_constraints(N, F),
            maplist(plain_labeling(N), T),
            visibility(T, F, Top, Bottom, Left, Right).


% ambiguous:

ambiguous(N, C, T1, T2) :-
            C = counts(Top, Bottom, Left, Right),
            ntower(N, T1, C),
            ntower(N, T2, C),
            T1 \= T2.

% speedup

ntower_speedup(T) :-
  statistics(runtime, [Start|_]),
  ntower(5, _, counts([2,1,2,4, 2],[2,4,3,1,2],[2,3,3,1,3],[2,3,1,4,2])),
  statistics(runtime, [End|_]),
  T is End - Start.

plain_ntower_speedup(T) :-
  statistics(runtime, [Start|_]),
  plain_ntower(5, _, counts([2,1,2,4, 2],[2,4,3,1,2],[2,3,3,1,3],[2,3,1,4,2])),
  statistics(runtime, [End|_]),
  T is End - Start.

speedup(Ratio) :-
  ntower_speedup(Ntower),
  plain_ntower_speedup(Plain),
  Ratio is Plain / Ntower.


