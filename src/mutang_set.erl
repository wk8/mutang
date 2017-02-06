%% @doc TODO wkpo
-module(mutang_set).

-export([
    wkpo/0
]).

-on_load(init/0).

-include("mutang_nif.hrl").

wkpo() -> ?NOT_LOADED.

init() -> ?INIT_NIF.
