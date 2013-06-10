%% @author eluksan
%% @doc @todo Add description to sim1.


-module(sim1).
-compile([debug_info, export_all]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([sim/2, get_test_units/0, test_sim/0]).

-record(defender, {n, hp, hp_remaining}).
-record(attacker, {n, dmg_min, dmg_max, accuracy}).

%% sim/2 returns a dict of tuples ({N_attackers remaining, N_defenders_remaining, partial_HP_remaining}, propability of outcome)

sim(A = #attacker{}, D = #defender{}) ->
	sim_sub(A, D, 1).

get_test_units() -> 
	{
	 #attacker{n = 2, dmg_min = 15, dmg_max = 30, accuracy = 0.8},
	 #defender{n = 2, hp = 200, hp_remaining = 400}
	}.

test_sim() ->
	{A, D} = get_test_units(),
	dict:to_list(sim(A, D)).

%% ====================================================================
%% Internal functions
%% ====================================================================

% Erlangs missing ceil function
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

% Computes the binomial coefficient N over K
binom(N, 0) when is_integer(N), N >= 0->
	1;
binom(0, K) when is_integer(K), K >= 0->
	0;
binom(N, K) when is_integer(N), is_integer(K), (N >= 0), (K >= 1), (N >= K) ->
	binom(N, K, 1, 1).
binom(N, K, K, Acc) ->
	(Acc * (N-K+1)) div K;
binom(N, K, I, Acc) ->
	binom(N, K, I+1, (Acc * (N-I+1)) div I).

% Helper for merging prob dicts
add_prob(_, P1, P2) ->
	P1+P2.

% Computes part of the probability tree
sim_sub(A = #attacker{}, D = #defender{n = Nd}, Pin) ->
	{Nd_1, A_left1} = kill_defenders(A, D),
	if Nd_1 == 0 -> dict:from_list(lists:map(fun({A_left, P}) -> {{A_left, 0, 0}, P*Pin} end, A_left1));
	   Nd_1 == Nd -> sim_attacker_tree(Pin, A, D);
	   true -> lists:map(fun({Na2, P}) -> sim_sub(A#attacker{n = Na2}, D#defender{n = Nd_1}, Pin*P) end, A_left1)
	end.


% Naive top-down aproach of computing possible combat outcome. O(n^b)
% Returns prob dict with {N attackers remaining, N defenders remaining, HP remaining} -> probability of outcome
sim_attacker_tree(P, #attacker{n = Na}, #defender{n = 0}) -> % no attackers or defenders remaining
	dict:from_list([{{Na, 0, 0}, P}]);

sim_attacker_tree(P, #attacker{n = 0}, #defender{n = Nd, hp_remaining = HL}) 
  when Nd > 0 ->
	dict:from_list([{{0, Nd, HL}, P}]);

% one attacker always kills one defender
sim_attacker_tree(P, #attacker{n = Na, dmg_min = Min}, #defender{n = Nd, hp = HP})
  when Min >= HP, Na > 0, Nd > 0 ->
	if
		Nd > Na -> HP_left = HP;
		Nd =< Na -> HP_left = 0
	end,
	dict:from_list([{{max(0, Na-Nd), max(0, Nd-Na), HP_left}, P}]);

% kill one unit with accumulated damage
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min}, D = #defender{n = Nd, hp = HP, hp_remaining = HL})
  when Min >= HL, Na > 0, Nd > 0 ->
	sim_attacker_tree(P, A#attacker{n = Na-1}, D#defender{n = Nd-1, hp_remaining = HP});

% only accumulate damage, to low dmg to kill
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min, dmg_max = Max, accuracy = Acc}, D = #defender{n = Nd, hp_remaining = HL})
  when Max < HL, Na > 0, Nd > 0 ->
	R1 = sim_attacker_tree((1-Acc)*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Min}),
	R2 = sim_attacker_tree(Acc*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Max}),
	dict:merge(fun add_prob/3, R1, R2);

% kill on hit, damage on miss
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min, dmg_max = Max, accuracy = Acc}, D = #defender{n = Nd, hp = HP, hp_remaining = HL})
  when Min < HL, Max >= HL, Na > 0, Nd > 0 ->
	R1 = sim_attacker_tree((1-Acc)*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Min}), % miss
	R2 = sim_attacker_tree(Acc*P, A#attacker{n = Na-1}, D#defender{n = Nd-1, hp_remaining = HP}), % hit
	dict:merge(fun add_prob/3, R1, R2).


% Remove defenders that will die for sure, due to number of attackers
% Return tuple {# of defenders remaining, probability list with number of remaining attackers}
kill_defenders(A = #attacker{n = Na, dmg_min = Min}, D = #defender{n = Nd, hp = HP}) ->
	Kill_prob = kill_one_defender(A, D),
	D_loss = min(trunc(Na/ceiling(HP/Min)), Nd),
	A_lost = kill_defenders1(Kill_prob, D_loss, Kill_prob),
	A_left = lists:map(fun({Aloss, P}) -> {Na-Aloss, P} end, A_lost),
	{Nd - D_loss, A_left}.

% return prob list of how many attackers are lost by killing Nd defenders,
% given the attacker loss for killing one defender in Plist
kill_defenders1(_, 0, _) ->
	[{0, 1}]; % No attackers are lost if we don't kill anything
kill_defenders1(_, Nd, Acc) when Nd == 1 ->
	Acc;
kill_defenders1(Plist, Nd, Acc) ->
	X1 = lists:map(fun({A_lost_plist, P_plist}) -> % list of dicts
				dict:from_list(lists:map(fun({A_lost_acc, P_acc}) -> {A_lost_plist+A_lost_acc, P_plist*P_acc} end, Acc))
		end, Plist),
	X2 = dict:to_list(lists:foldl(fun(D, Acc1) -> dict:merge(fun add_prob/3, D, Acc1) end, dict:new(), X1)),
	kill_defenders1(Plist, Nd-1, X2).

%kill_one_defender(_,_) ->
%	[{300, 1}];
% How many attackers are lost after killing one defender
kill_one_defender(A = #attacker{dmg_min = Min, dmg_max = Max}, D = #defender{hp = HP}) ->
	Na_min = ceiling(HP/Min),
	Na_max = ceiling(HP/Max),
	X1 = sim_attacker_tree(1, A#attacker{n = Na_min}, D#defender{n = 1}), %inefficient, since there is no ordering of the attacks
	X = dict:fold(fun({An, 0, 0}, P, Acc) -> [{Na_min-An, P}|Acc] end, [], X1), % there should never be any surviving defender, thus {An, 0, 0}
	%X = kill_one_defender1(Na_max, 0, Min, Max, Acc, HP, dict:new()),
	lists:keysort(1, dict:to_list(X)).

% Hit: nuber of hit attacks, Miss: number of missed attacks, Min/Max: dmg inflicted
% HP: initial hitpoints
% Pdict: accumulating probability dict of [{lost attackers -> prob}]
kill_one_defender1(0, Miss, Min, _, Accuracy, HP, Pdict) 
  when Miss*Min >= HP -> 
	dict:update_counter(Miss, math:pow(1-Accuracy, Miss), Pdict);

kill_one_defender1(Hit, Miss, Min, Max, Accuracy, HP, Pdict)
  when Hit*Max + Miss*Min < HP ->
	kill_one_defender1(Hit, Miss+1, Min, Max, Accuracy, HP, Pdict);

kill_one_defender1(Hit, Miss, Min, Max, Accuracy, HP, Pdict) ->
	kill_one_defender1(Hit-1, Miss, Min, Max, Accuracy, HP,
					   dict:update_counter(Hit+Miss, binom(Hit+Miss, Miss)*math:pow(Accuracy, Hit)*math:pow(1-Accuracy, Miss), Pdict)).
	