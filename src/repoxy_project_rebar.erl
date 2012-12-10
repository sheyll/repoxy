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
    ok = application:load(rebar),
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
-spec rebar(#prj_cfg{}, [atom()] | atom()) ->
                   ok | {error, term()}.
rebar(Cfg, RebarCmds) ->
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
        rebar_core:process_commands(RebarCmds1, Cfg#prj_cfg.rebar_cfg),
        ok
    catch
        C:E ->
            {error, {{rebar_failed, RebarCmds}, C, E}}
    end.
