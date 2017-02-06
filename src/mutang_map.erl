%% @doc TODO wkpo
-module(mutang_map).

-export([
    new/0,
    put/3,
    get/2
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

put(Key, Value, Map) ->
    nif_wrapper(Map, fun nif_put/4, [Key, Value],
                fun(NewVersion) -> Map#?MODULE{version = NewVersion} end).

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

%%% End of NIF functions
