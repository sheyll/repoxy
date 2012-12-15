-module(repoxy_project_rebar).
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
            {error, {{rebar_failed, RebarCmds}, C, E}}
    end.
