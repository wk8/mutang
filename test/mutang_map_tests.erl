-module(mutang_map_tests).

-include_lib("eunit/include/eunit.hrl").

%% TODO wkpo include all tests from OTP code!

basic_put_get_test_() ->
    Map1 = mutang_map:new(),
    Map2 = mutang_map:put(12, 28, Map1),
    Map3 = mutang_map:put(1, 2, Map2),
    Map4 = mutang_map:put(12, 82, Map3),

    [
        ?_assertEqual(82, mutang_map:get(12, Map4)),
        ?_assertEqual(2, mutang_map:get(1, Map4)),
        ?_assertError({badkey, 99}, mutang_map:get(99, Map4))
    ].

basic_wrong_version_test_() ->
    Map1 = mutang_map:new(),
    _Map2 = mutang_map:put(12, 28, Map1),

    ?_assertError({badversion, 1, 0}, mutang_map:get(99, Map1)).

basic_badmap_test_() ->
    ?_assertError({badmap, i_aint_a_map}, mutang_map:get(99, i_aint_a_map)).

basic_badkey_test_() ->
    ?_assertError({badkey, 99}, mutang_map:get(99, mutang_map:new())).
