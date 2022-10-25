split(-1, _, (-1, -1)) :- !.

split(node(KY, VL, PR, LN, RN), K, R) :-
	K > KY, split(RN, K, (R1, R2)), R = (node(KY, VL, PR, LN, R1), R2), !.

split(node(KY, VL, PR, LN, RN), K, R) :-
	split(LN, K, (L1, L2)), R = (L1, node(KY, VL, PR, L2, RN)), !.

merge(L1, -1, L1) :- !.
merge(-1, R1, R1) :- !.

merge(node(KY1, VL1, PR1, LN1, RN1), node(KY2, VL2, PR2, LN2, RN2), R) :-
	PR1 > PR2, merge(RN1, node(KY2, VL2, PR2, LN2, RN2), TRN1),
	R = node(KY1, VL1, PR1, LN1, TRN1), !.

merge(node(KY1, VL1, PR1, LN1, RN1), node(KY2, VL2, PR2, LN2, RN2), R) :-
	merge(node(KY1, VL1, PR1, LN1, RN1), LN2, TLN2),
	R = node(KY2, VL2, PR2, TLN2, RN2), !.

map_put(-1, K, V, R) :- rand_int(20000, NPR), R = node(K, V, NPR, -1, -1), !.

map_put(node(K, V, PR, LN, RN), K, V, node(K, V, PR, LN, RN)) :- !.

map_put(node(KY, VL, PR, LN, RN), K, V, R) :-
	map_get(node(KY, VL, PR, LN, RN), K, _), map_remove(node(KY, VL, PR, LN, RN), K, R1),
	map_put(R1, K, V, R), !.

map_put(node(KY, VL, PR, LN, RN), K, V, R) :-
	split(node(KY, VL, PR, LN, RN), K, (R1, R2)), rand_int(20000, NPR),
	merge(R1, node(K, V, NPR, -1, -1), TR),
	merge(TR, R2, R), !.

map_builder(CURT, [], CURT) :- !.

map_builder(CURT, [(HMK, HMV) | TM], R) :-
	map_put(CURT, HMK, HMV, TR), map_builder(TR, TM, R), !.

map_build(LM, R) :-
	map_builder(-1, LM, R), !.

map_remove(-1, K, -1) :- !.

map_remove(node(KY, VL, PR, LN, RN), K, R) :-
	K =:= KY, merge(LN, RN, R), !.

map_remove(node(KY, VL, PR, LN, RN), K, R) :-
	K > KY, map_remove(RN, K, TRN), R = node(KY, VL, PR, LN, TRN), !.

map_remove(node(KY, VL, PR, LN, RN), K, R) :-
	map_remove(LN, K, TLN), R = node(KY, VL, PR, TLN, RN), !.

map_get(node(K, V, _, _, _), K, V) :- !.

map_get(node(KY, VL, _, _, RN), K, V) :-
	K > KY, map_get(RN, K, V), !.

map_get(node(KY, VL, _, LN, _), K, V) :-
	map_get(LN, K, V), !.

map_floorKey(node(KY, VL, PR, LN, -1), K, FK) :-
	KY =< K, FK = KY, !.

map_floorKey(node(KY, VL, PR, LN, RN), K, FK) :-
	KY =:= K, FK = KY, !.

map_floorKey(node(KY, VL, PR, LN, RN), K, FK) :-
	KY > K, map_floorKey(LN, K, FK), !.

map_floorKey(node(KY, VL, PR, _, node(RKY, RVL, RPR, RLN, RRN)), K, FK) :-
	KY < K, KY =< RKY, map_floorKey(node(RKY, RVL, RPR, RLN, RRN), K, FK), !.

map_floorKey(node(KY, VL, PR, LN, RN), K, FK) :-
	KY < K, FK = KY, !.