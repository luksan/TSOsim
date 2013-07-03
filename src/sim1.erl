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

% Don't forget about the general when comparing results!


%% sim/2 returns a dict of tuples ({N_attackers remaining, N_defenders_remaining, partial_HP_remaining}, propability of outcome)
sim(A = #attacker{}, D = #defender{}) ->
	dict:from_list(sim_sub(A, D, 1)).

%% ==============
%% Test functions
%% ==============
get_test_units() -> 
	HP = 40,
	{
	 #attacker{n = 200, dmg_min = 0, dmg_max = 20, accuracy = 0.8},
	 #defender{n = 10, hp = HP, hp_remaining = HP}
	}.

test_sim() ->
	{A, D} = get_test_units(),
	%X = dict:to_list(sim(A, D)),
	X = dict:to_list(sim_zero_min_dmg(A, D)),
	lists:reverse(lists:keysort(2, X)).

diff_lists(A, B) ->
	A1 = lists:sort(A),
	B1 = lists:sort(B),
	lists:zipwith(fun({An,P1}, {An,P2}) -> {An,(P1-P2)/P1} end, A1, B1).

sum_prob(Plist) ->
	lists:foldl(fun({_, P}, Acc) -> P+Acc end, 0, Plist).

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

div_up(A, B) when is_integer(A), is_integer(B) -> (A+B-1) div B;
div_up(A, B) -> ceiling(A/B).

% Computes the binomial coefficient N over K
binom(K, K) -> 1;
binom(N, 0) when is_integer(N), N > 0 -> 1;
binom(0, K) when is_integer(K), K > 0 -> 0;
binom(N, 1) -> N;
binom(N, K) when is_integer(N), is_integer(K), (N >= 0), (K >= 1), (N >= K) ->
	binom(N, K, 1, 1).

binom(N, K, K, Acc) -> (Acc * (N-K+1)) div K;
binom(N, K, I, Acc) -> binom(N, K, I+1, (Acc * (N-I+1)) div I).

% probability of making K trials before R fails are observed,
% with success probability P
neg_binom_pmf(K, R, P) when K >= R ->
	binom(K-1, K-R)*math:pow(1-P, R)*math:pow(P, K-R).

szmd_p(Na_hit, #attacker{n = Na, accuracy = Acc}) ->
	binom(Na, Na_hit)*math:pow(Acc, Na_hit)*math:pow(1-Acc, Na-Na_hit).

sim_zero_min_dmg(A = #attacker{n = Na, dmg_min = 0}, D = #defender{}) ->
	sim_zero_min_dmg(Na, A, D, dict:new()).

sim_zero_min_dmg(_Na_hit, #attacker{n = Na}, #defender{n = 0}, Pdict) ->
	dict:store({Na, 0, 0}, 1, Pdict);
sim_zero_min_dmg(0, #attacker{n = Na, accuracy = Acc}, #defender{n = Nd, hp_remaining = HP}, Pdict) ->
	dict:store({0, Nd, HP}, math:pow(1-Acc, Na), Pdict);
sim_zero_min_dmg(Na_hit, A = #attacker{dmg_max = Dmg}, D = #defender{n = Nd, hp_remaining = HPr}, Pdict) 
  when Na_hit*Dmg < HPr ->
	P = dict:store({0, Nd, HPr-Na_hit*Dmg}, szmd_p(Na_hit, A), Pdict),
	sim_zero_min_dmg(Na_hit-1, A, D, P);
sim_zero_min_dmg(Na_hit, A = #attacker{dmg_max = Dmg}, D = #defender{n = Nd, hp = HP, hp_remaining = HPr}, Pdict)
  when (HPr + Dmg -1) div Dmg + ((HP + Dmg -1) div Dmg)*(Nd-1) > Na_hit ->
	Aloss1 = div_up(HPr, Dmg),
	ApD = div_up(HP, Dmg),
	Dloss2 = (Na_hit-Aloss1) div ApD,
	Admg = Na_hit - Aloss1 - Dloss2 * ApD,
	P = dict:store({0, Nd-1-Dloss2, HP-Admg*Dmg}, szmd_p(Na_hit, A), Pdict),
	sim_zero_min_dmg(Na_hit-1, A, D, P);
sim_zero_min_dmg(Na_tot, A = #attacker{n = Na, dmg_max = Dmg, accuracy = Acc}, D = #defender{n = Nd, hp = HP, hp_remaining = HPr}, Pdict) ->
	Na_hit = div_up(HPr, Dmg) + div_up(HP, Dmg)*(Nd-1),
	Pd2 = dict:store({Na-Na_tot, 0, 0}, neg_binom_pmf(Na_tot, Na_hit, 1-Acc), Pdict),
	sim_zero_min_dmg(Na_tot-1, A, D, Pd2).

% Helper for merging prob dicts
add_prob(_, P1, P2) ->
	P1+P2.

merge_pdict_list(List) ->
	lists:foldl(fun(Pdict, Acc) -> dict:merge(fun add_prob/3, Pdict, Acc) end, dict:new(), List).

merge_ordpdict_list(List) ->
	lists:foldl(fun(Pdict, Acc) -> orddict:merge(fun add_prob/3, Pdict, Acc) end, orddict:new(), List).

% Compute the multinomial coefficient N over [k1,k2,...,kn]
multinom(N, K) ->
	multinom(N, K, 0, 1).
multinom(_N, [], _Kprev, Acc) ->
	Acc;
multinom(N, [Ki|K], K_prev_sum, Acc) ->
	multinom(N, K, Ki+K_prev_sum, Acc * binom(Ki+K_prev_sum, Ki)).

% Computes part of the probability tree
% returns a dict of tuples ({N_attackers remaining, N_defenders_remaining, partial_HP_remaining}, propability of outcome)
sim_sub(#attacker{n = Na}, #defender{n = Nd, hp_remaining = HL}, Pin)
  when Na == 0; Nd == 0 ->
	%dict:from_list([{{Na, Nd, HL}, Pin}]);
	[{{Na, Nd, HL}, Pin}];

sim_sub(A = #attacker{}, D = #defender{n = Nd, hp = HP}, Pin) ->
	case kill_defenders(A, D) of % remove sure kills
		{0, A_left1} -> % No defenders remaining
			orddict:from_list(lists:map(fun({A_left, P}) -> {{A_left, 0, 0}, P*Pin} end, A_left1));
		{Nd, _} -> % No sure kills remaining
			X = lists:map(fun({{Na_2, Nd_2, HP_left}, P}) -> % list of Pdicts from sim_sub/3
								   sim_sub(A#attacker{n = Na_2}, D#defender{n = Nd_2, hp_remaining = HP_left}, Pin*P)
						   end, attack_one_defender(A, D)),
			merge_ordpdict_list(X);
		{Nd_1, A_left1} -> % some sure kills remaining
			X = lists:map(fun({Na2, P}) -> sim_sub(A#attacker{n = Na2}, D#defender{n = Nd_1, hp_remaining = HP}, Pin*P) end, A_left1),
			merge_ordpdict_list(X)
	end.


% Naive top-down aproach of computing possible combat outcome. O(2^A)
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

% Returns the minimum number of attackers needed to get a sure kill.
min_attackers(#attacker{dmg_min = Min}, #defender{hp = _}) when Min == 0 ->
	5000; % Any number larger than the maximum number of units that can be assigen to a gen. Ideally +Inf
min_attackers(#attacker{dmg_min = Min}, #defender{hp = HP}) when Min > 0 ->
	ceiling(HP/Min).

% Remove defenders that will die for sure, due to number of attackers
% Return tuple {# of defenders remaining, probability list with number of remaining attackers}
kill_defenders(A = #attacker{n = Na}, D = #defender{n = Nd}) ->
	Kill_prob = kill_one_defender(A, D),
	D_loss = min(trunc(Na/min_attackers(A, D)), Nd),
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
					   orddict:from_list(lists:map(fun({A_lost_acc, P_acc}) ->
													{A_lost_plist+A_lost_acc, P_plist*P_acc}
												end,
									  			Acc))
				   end, Plist),
	X2 = orddict:to_list(merge_ordpdict_list(X1)),
	kill_defenders1(Plist, Nd-1, X2).

% How many attackers are lost after killing one defender
kill_one_defender(A = #attacker{n = Na}, D = #defender{hp = HP}) ->
	Na_min = min_attackers(A, D),
	if Na_min =< Na -> % can't attack with more than 250 units, saves processor time
		   X = attack_one_defender1(0, Na_min, A#attacker{n = Na_min}, HP, dict:new());
	   true ->
		   X = dict:new()
	end,
	dict:fold(fun({An, 0}, P, Acc) -> [{An, P} | Acc] end, [], X).

% Calculate the possible outcomes of attacking one defender
% Return prob list with [ {{attackers remaining, defenders remaining, hp_remaining}, P}  | ... ]
attack_one_defender(A = #attacker{n = Na}, D = #defender{n = Nd, hp = HP, hp_remaining = HL})
  when Na > 0, Nd > 0 ->
	Na_min = min(min_attackers(A, D), Na),
	X = attack_one_defender1(0, Na_min, A, HL, dict:new()),
	dict:fold(fun({Na_lost, Dmg}, P, Acc) when Na_lost =< Na ->
					  if Dmg == 0, Nd > 1 -> [{{Na-Na_lost, Nd-1, HP}, P} | Acc];
						 Dmg == 0, Nd == 1 -> [{{Na-Na_lost, 0, 0}, P} | Acc];
						 Dmg > 0, Dmg < HL -> [{{Na-Na_lost, Nd, HL-Dmg}, P} | Acc]
					  end
			  end, [], X).
	%lists:keysort(1, dict:to_list(X)).

% Initially called with Miss set to equal or less than the bare minimum to get a sure kill, ie min(ceil(HP/Min), Na)
% Hit: number of hit attacks, Miss: number of missed attacks, Min/Max: dmg inflicted per attack
% HP: initial hitpoints
% Pdict: accumulating probability dict of [{lost attackers, accumulated damage} -> prob}]
%        accumulated damage is set to zero if the unit is killed 
attack_one_defender1(Hit, _, #attacker{n = Na}, _, Pdict) 
  when Hit > Na ->
	Pdict;

attack_one_defender1(Hit, Miss, #attacker{dmg_max = Max, accuracy = Accuracy}, HP, Pdict) 
  when Miss == 0, Hit*Max >= HP -> 
	dict:update_counter({Hit, 0}, math:pow(Accuracy, Hit), Pdict);

attack_one_defender1(Hit, Miss, A=#attacker{n = Na}, HP, Pdict)
  when Miss > 0, Hit + Miss > Na ->
	attack_one_defender1(Hit, Miss-1, A, HP, Pdict);

attack_one_defender1(Hit, Miss, A=#attacker{n = Na, dmg_min = Min, dmg_max = Max}, HP, Pdict)
  when Miss >= 0, Hit*Max + Miss*Min < HP, Hit + Miss == Na ->
	P = outcome_prob(Hit, Miss, A, HP),
	P1 = dict:update_counter({Hit+Miss, Hit*Max+Miss*Min}, P, Pdict),
	attack_one_defender1(Hit+1, Miss, A, HP, P1);

attack_one_defender1(Hit, Miss, A=#attacker{dmg_min = Min, dmg_max = Max}, HP, Pdict)
  when Miss >= 0, Hit*Max + Miss*Min < HP ->
	attack_one_defender1(Hit+1, Miss, A, HP, Pdict);

attack_one_defender1(Hit, Miss,  A=#attacker{dmg_min = Min, dmg_max = Max}, HP, Pdict) 
  when Miss > 0, Hit*Max + Miss*Min >= HP ->
	P = outcome_prob(Hit, Miss, A, HP),
	P1 = dict:update_counter({Hit+Miss, 0}, P, Pdict),
	attack_one_defender1(Hit, Miss-1, A, HP, P1).


% Helper to calculate the combined probability of a certaion attack result, with permutations
outcome_prob(Hit, Miss, #attacker{dmg_min = Min, dmg_max = Max, accuracy = Accuracy}, HP)
  when Miss >= 0, Hit >= 0 ->
	if Hit*Max + (Miss-1)*Min >= HP, Miss > 0 -> Perm_last = 1;
	   true -> Perm_last = 0
	end,
	B = binom(Hit+Miss-Perm_last, max(Hit-Perm_last, 0)),
	H = math:pow(Accuracy, Hit),
	M = math:pow(1-Accuracy, Miss),
	B*H*M.

% cd('H:/workspace/TSOsim/src'), c(sim1, [debug_info]), rr(sim1).
% c(sim1, [debug_info]).
% {A,D} = sim1:get_test_units().
