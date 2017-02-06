-module(map_benchmark).

-export([
    put/2,
    get/2
]).

%% @doc Benchmarks how long it takes to `put' `N' random elements into
%% erlang's standard `maps', `dict's, and `mutang_map's; does `Reps' repetitions
put(N, Reps) ->
    RandomList = build_random_list(N),

    io:format("Benchmarking putting ~B items into a map ~n", [N]),
    benchmark(put_benchmarking_function(maps, RandomList), Reps),

    io:format("Benchmarking putting ~B items into a dict ~n", [N]),
    benchmark(put_benchmarking_function(dict, RandomList), Reps),

    io:format("Benchmarking putting ~B items into a mutang_map ~n", [N]),
    benchmark(put_benchmarking_function(mutang_map, RandomList), Reps).

%% @doc Benchmarks how long it takes to retrieve all the items from the
%% different maps
get(N, Reps) ->
    RandomList = build_random_list(N),

    Map = erlang:apply(put_benchmarking_function(maps, RandomList), []),
    Dict = erlang:apply(put_benchmarking_function(dict, RandomList), []),
    MutangMap = erlang:apply(put_benchmarking_function(mutang_map, RandomList), []),

    ShuffledRandomList = [X || {_, X} <- lists:sort([ {rand:uniform(), I} || I <- RandomList])],

    io:format("Benchmarking getting ~B items from a map ~n", [N]),
    benchmark(get_benchmarking_function(maps, Map, ShuffledRandomList), Reps),

    io:format("Benchmarking getting ~B items from a dict ~n", [N]),
    benchmark(get_benchmarking_function(dict, Dict, ShuffledRandomList), Reps),

    io:format("Benchmarking getting ~B items from a mutang_map ~n", [N]),
    benchmark(get_benchmarking_function(mutang_map, MutangMap, ShuffledRandomList), Reps).

%%% Private helpers

benchmark(Fun, Reps) ->
    erlang:garbage_collect(),
    Times = test_loop(Fun, Reps, []),

    Length = erlang:length(Times),
    Min = lists:min(Times),
    Max = lists:max(Times),
    Med = lists:nth(round((Length / 2)), lists:sort(Times)),
    Total = lists:foldl(fun(Time, Sum) -> Time + Sum end, 0, Times),
    Avg = erlang:round(Total / Length),
    io:format("Range: ~b - ~b mics~n"
              "Median: ~b mics~n"
              "Average: ~b mics~n",
              [Min, Max, Med, Avg]).

build_random_list(N) -> build_random_list(N, []).
build_random_list(0, Acc) -> Acc;
build_random_list(N, Acc) -> build_random_list(N - 1, [{random_int(), random_int()} | Acc]).

test_loop(_Fun, 0, Times) ->
    Times;
test_loop(Fun, Reps, Times) ->
    {Time, _Result} = timer:tc(Fun),
    test_loop(Fun, Reps - 1, [Time | Times]).

-define(RANDOM_RANGE_WIDTH, 10000).

random_int() ->
    rand:uniform(?RANDOM_RANGE_WIDTH) - erlang:trunc(?RANDOM_RANGE_WIDTH / 2).

put_benchmarking_function(maps, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, Value}, Map) ->
                maps:put(Key, Value, Map)
            end,
            #{},
            RandomList
        )
    end;
put_benchmarking_function(dict, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, Value}, Dict) ->
                dict:store(Key, Value, Dict)
            end,
            dict:new(),
            RandomList
        )
    end;
put_benchmarking_function(mutang_map, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, Value}, MutangMap) ->
                mutang_map:put(Key, Value, MutangMap)
            end,
            mutang_map:new(),
            RandomList
        )
    end.

get_benchmarking_function(maps, Map, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, _Value}, _PreviousValue) ->
                maps:get(Key, Map)
            end,
            undefined,
            RandomList
        )
    end;
get_benchmarking_function(dict, Dict, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, _Value}, _PreviousValue) ->
                dict:fetch(Key, Dict)
            end,
            undefined,
            RandomList
        )
    end;
get_benchmarking_function(mutang_map, MutangMap, RandomList) ->
    fun() ->
        lists:foldl(
            fun({Key, _Value}, _PreviousValue) ->
                mutang_map:get(Key, MutangMap)
            end,
            undefined,
            RandomList
        )
    end.