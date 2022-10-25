prime(2).
composite(1).

prime(X) :- \+ composite(X).

loop(Offset, Start, End) :- Start > End, !.
loop(Offset, Start, End) :-
  assert(composite(Start)), Ii is Start + Offset, loop(Offset, Ii, End).

resheto(I) :- composite(I), !.
resheto(I) :- I1 is I + I, loop(I, I1, 100000).

fill_resheto(R, N) :- R > N, !.
fill_resheto(R, N) :-
  resheto(R), R1 is R + 1, fill_resheto(R1, N).


init(N) :-
N1 is sqrt(N), fill_resheto(2, N1).


equal_lists([], []) :- !.
equal_lists([H1 | T1], [H2 | T2]) :-
	H1 = H2,
	equal_lists(T1, T2).

check(L) :- reverse(L, L1), equal_lists(L, L1).

is_palindrome(N, C, L) :- N = 0, !, check(L).
is_palindrome(N, C, L) :-
	N1 is div(N, C),
	N2 is mod(N, C),
	is_palindrome(N1, C, [N2 | L]).

prime_palindrome(N, C) :- prime(N), is_palindrome(N, C, []).


prime_divisors(1, []) :- !.

prime_divisors(N, [H | T]) :-
  number(N), number(H), !,
  prime(H), N1 is div(N, H), 0 =:= mod(N, H),
  prime_divisors(N1, T).

kostyl(1, P, []) :- !.
kostyl(N, P, [H | T]) :-
  (\+ number(N)), !, H >= P,
  kostyl(N1, H, T), N is N1 * H.

prime_divisors(N, [H | T]) :- (\+ number(N)), !, kostyl(N, 0, [H | T]).


generate_primes(N, P, [N]) :-
    P1 is P * P, N < P1, !.
generate_primes(N, P, L) :-
    (\+ (0 is mod(N, P))), !,
    P1 is P + 1, generate_primes(N, P1, L), !.
generate_primes(N, P, [P | T]) :-
    0 is mod(N, P), !,
    N1 is div(N, P), generate_primes(N1, P, T), !.


prime_divisors(N, L) :-
	generate_primes(N, 2, L).





