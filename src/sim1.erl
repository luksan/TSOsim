%% @author eluksan
%% @doc @todo Add description to sim1.


-module(sim1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([sim/2, get_test_units/0]).

-record(defender, {n, hp, hp_remaining}).
-record(attacker, {n, dmg_min, dmg_max, accuracy}).

%% sim/2 returns a dict of tuples ({N_attackers remaining, N_defenders_remaining, partial_HP_remaining}, propability of outcome)

sim(A = #attacker{}, D = #defender{}) ->
	sim_sub(A, D, 1).

get_test_units() -> 
	{
	 #attacker{n = 10, dmg_min = 15, dmg_max = 30, accuracy = 0.8},
	 #defender{n = 1, hp = 40, hp_remaining = 40}
	}.
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
sim_attacker_tree(P, #attacker{n = 0}, #defender{n = N}) ->% no attackers or defenders remaining
	dict:from_list([{{0, N}, P}]);
sim_attacker_tree(P, #attacker{n = N}, #defender{n = 0}) ->
	dict:from_list([{{N, 0}, P}]);

% one attacker always kills one defender
sim_attacker_tree(P, #attacker{n = Na, dmg_min = Min}, #defender{n = Nd, hp = HP})
  when Min >= HP ->
	dict:from_list([{{max(0, Na-Nd), max(0, Nd-Na)}, P}]);

% kill one unit with accumulated damage
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min}, D = #defender{n = Nd, hp = HP, hp_remaining = HL})
  when Min >= HL ->
	sim_attacker_tree(P, A#attacker{n = Na-1}, D#defender{n = Nd-1, hp_remaining = HP});

% only accumulate damage, to low dmg to kill
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min, dmg_max = Max, accuracy = Acc}, D = #defender{hp_remaining = HL})
  when Max < HL ->
	R1 = sim_attacker_tree((1-Acc)*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Min}),
	R2 = sim_attacker_tree(Acc*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Max}),
	dict:merge(fun add_prob/3, R1, R2);

% kill on hit, damage on miss
sim_attacker_tree(P, A = #attacker{n = Na, dmg_min = Min, dmg_max = Max, accuracy = Acc}, D = #defender{n = Nd, hp = HP, hp_remaining = HL})
  when Min < HL, Max >= HL ->
	R1 = sim_attacker_tree((1-Acc)*P, A#attacker{n = Na-1}, D#defender{hp_remaining = HL-Min}), % miss
	R2 = sim_attacker_tree(Acc*P, A#attacker{n = Na-1}, D#defender{n = Nd-1, hp_remaining = HP}), % hit
	dict:merge(fun add_prob/3, R1, R2).

% Remove defenders that will die for sure, due to number of attackers
% Return tuple {# of defenders remaining, probability list with number of remaining attackers}
kill_defenders(A = #attacker{n = Na, dmg_min = Min}, D = #defender{n = Nd, hp = HP}) ->
	Kill_prob = kill_one_defender(A, D),
	D_loss = trunc(Na/ceiling(HP/Min)),
	D_remain = Nd - D_loss,
	A_lost = kill_defenders1(Kill_prob, D_loss, Kill_prob),
	A_left = lists:map(fun({Aloss, P}) -> {Na-Aloss, P} end, A_lost),
	{D_remain, A_left}.

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
	
% How many attackers are lost after killing one defender
kill_one_defender(A = #attacker{dmg_min = Min}, D = #defender{hp = HP}) ->
	Na_min = ceiling(HP/Min),
	X = sim_attacker_tree(1, A#attacker{n = Na_min}, D#defender{n = 1}), %inefficient, since there is no ordering of the attacks
	dict:fold(fun({An, 0}, P, Acc) -> [{Na_min-An, P}|Acc] end, [], X). % there should never be any surviving defender, thus {An, 0}
