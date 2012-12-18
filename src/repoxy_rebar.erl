-module(repoxy_rebar).
-compile([export_all]).

-include("repoxy.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Start rebar the first time, initialize the configuration and do all necessary
%% setup work. The return value must be passed into all subsequent calls.
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar(string()) -> rebar_config:config().
load_rebar(ProjectDir) ->
    case application:load(rebar) of
        ok -> ok;
        {error,{already_loaded,rebar}} -> ok
    end,
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,
    Cfg = repoxy_rebar_cfg:default(ProjectDir),
    rebar_log:init(Cfg),
    Cfg.

%%------------------------------------------------------------------------------
%% @doc
%% Clean, get-deps, compile then as extra step repoxy_discover, even if there
%% were error.
%% @end
%%------------------------------------------------------------------------------
-spec discover(rebar_config:config()) ->
                      ok | {error, term()}.
discover(RebarCfg) ->
    rebar(RebarCfg, ['clean', 'get-deps', 'compile']),
    rebar(RebarCfg, ['repoxy_discover']).

%%------------------------------------------------------------------------------
%% @doc
%% Run rebar command.
%% @end
%%------------------------------------------------------------------------------
-spec rebar(rebar_config:config(), [atom()] | atom()) ->
                   ok | {error, term()}.
rebar(RebarCfg, RebarCmds) ->
    RebarCmds1 = if
                     is_list(RebarCmds) ->
                         if
                             is_integer(hd(RebarCmds)) ->
                                 [list_to_atom(RebarCmds)];
                             true ->
                                 [if is_list(C) ->
                                          list_to_atom(C);
                                     true ->
                                          C
                                  end
                                  || C <- RebarCmds]
                         end;
                     is_atom(RebarCmds) ->
                         [RebarCmds]
                 end,
    try
        rebar_core:process_commands(RebarCmds1, RebarCfg),
        ok
    catch
        C:E ->
            error_logger:error_msg("Rebar commands: ~p~n ~w: ~p~n", [RebarCmds1, C, E]),
            {error, {{rebar_failed, RebarCmds}, C, E}}
    end.
