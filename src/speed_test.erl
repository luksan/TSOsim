%% @author eluksan
%% @doc @todo Add description to speed_test.


-module(speed_test).
-compile([export_all]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([bench_test_sim/1, bench_update_counter/2]).

bench_test_sim(N) -> 
	Tus = bench_test_sim(N, 0)/N,
	io:format("Avg time: ~.1f ms.~n", [Tus/1000]).

bench_test_sim(0, Acc) ->
	Acc;
bench_test_sim(N, Acc) ->
	{T, _} = timer:tc(fun sim1:test_sim/0),
	bench_test_sim(N-1, Acc + T).

bench_update_counter(Iter, Nkeys) ->
	Tdict = time_duc(Iter, Nkeys, 0, dict:new()),
	Torddict = time_oduc(Iter, Nkeys, 0, orddict:new()),
	io:format("Avg time, dict:    ~.1f us.~n", [Tdict+0.0]),
	io:format("Avg time, orddict: ~.1f us.~n", [Torddict+0.0]).

%% ====================================================================
%% Internal functions
%% ====================================================================

time_duc(0, _Nkeys, Ttot, _Dict) ->
	Ttot;
time_duc(Iter, Nkeys, Ttot, Dict) ->
	{T, Dict2} = timer:tc(?MODULE, dict_update_counter, [lists:seq(1, Nkeys), Dict]),
	time_duc(Iter-1, Nkeys, Ttot+T, Dict2).

time_oduc(0, _Nkeys, Ttot, _Dict) ->
	Ttot;
time_oduc(Iter, Nkeys, Ttot, Dict) ->
	{T, Dict2} = timer:tc(?MODULE, orddict_update_counter, [lists:seq(1, Nkeys), Dict]),
	time_oduc(Iter-1, Nkeys, Ttot+T, Dict2).

dict_update_counter([], Dict) ->
	Dict;
dict_update_counter([Key|Keys], Dict) ->
	dict_update_counter(Keys, dict:update_counter(Key, 1, Dict)).

orddict_update_counter([], Dict) ->
	Dict;
orddict_update_counter([Key|Keys], Dict) ->
	orddict_update_counter(Keys, orddict:update_counter(Key, 1, Dict)).

