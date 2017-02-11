%% @doc TODO wkpo
-module(mutang_map).

-export([
    new/0,
    put/3,
    get/2,
    wkpo/0,
    reset_wkpo/0,
    get_nsec/0,
    reset_nsec/0
]).

-on_load(init/0).

-include("mutang_nif.hrl").

-record(?MODULE, {
    pointer,
    version
}).

%% TODO wkpo specs?
%% TODO wkpo opaque type?

new() ->
    {Pointer, Version} = nif_new(),
    #?MODULE{pointer = Pointer, version = Version}.

put(Key, Value, #?MODULE{pointer = Pointer, version = Version} = Map) ->
    TimeBefore = wkpo(),
    {Time, {ok, NewVersion}} = timer:tc(
        fun() -> nif_put(Pointer, Version, Key, Value) end),
    erlang:put(wkpo, TimeBefore + Time),
    Map#?MODULE{version = NewVersion}.

wkpo() ->
    case erlang:get(wkpo) of
        undefined -> 0;
        V -> V
    end.

reset_wkpo() ->
    erlang:put(wkpo, 0).

get(Key, Map) ->
    nif_wrapper(Map, fun nif_get/3, [Key],
                fun(Value) -> Value end).

%% TODO wkpo other fun head for anything else..

%%% Private helpers

%% @private
%% @doc Wraps the calls to nifs to catch and format common errors
nif_wrapper(#?MODULE{pointer = Pointer, version = Version} = Map, NifFun, Args, SuccessFun) ->
    case erlang:apply(NifFun, [Pointer, Version | Args]) of
        {ok, SuccessValue} -> SuccessFun(SuccessValue);
        {error, badmap} -> erlang:error({badmap, Map});
        {error, badversion, ActualVersion} -> erlang:error({badversion, ActualVersion, Version});
        {error, badkey} -> [Key | _Rest] = Args, erlang:error({badkey, Key})
    end;
nif_wrapper(NotAMap, _NifFun, _Args, _SuccessFun) ->
    erlang:error({badmap, NotAMap}).

%%% End of private helpers

%%% NIF functions

init() -> ?INIT_NIF.
nif_new() -> ?NOT_LOADED.
nif_put(_Pointer, _Version, _Key, _Value) -> ?NOT_LOADED.
nif_get(_Pointer, _Version, _Key) -> ?NOT_LOADED.

get_nsec() -> ?NOT_LOADED.
reset_nsec() -> ?NOT_LOADED.

%%% End of NIF functions
