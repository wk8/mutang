-define(INIT_NIF,
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), 0)).

-define(NOT_LOADED, erlang:nif_error({not_loaded, [{module, ?MODULE},
                                                   {line, ?LINE},
                                                   {function, ?FUNCTION_NAME, ?FUNCTION_ARITY}]})).
