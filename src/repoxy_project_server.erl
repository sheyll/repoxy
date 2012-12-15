%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% The server and gen_fsm callbacks for {@link repoxy_project}.
%%% @end
%%% Created :  8 Dec 2012 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(repoxy_project_server).

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1,
         no_project_loaded/2,
         project_loaded/2,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-include("repoxy.hrl").
-include("repoxy_project_server.hrl").

-record(state, {node_backup :: #node_backup{} | no_node_backup,
                prj_cfg     :: #prj_cfg{} | no_prj_cfg}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, no_project_loaded, #state{}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
no_project_loaded(?load(Dir), _State) ->
    BuildDir = repoxy_project_code:new_build_dir(),
    NodeBackup = repoxy_project_code:backup_node(),
    RebarCfg = repoxy_project_rebar:load_rebar(Dir),
    repoxy_project_events:notify(?on_load),
    {next_state, project_loaded,
     #state{prj_cfg = #prj_cfg{build_dir = BuildDir,
                               rebar_cfg = RebarCfg},
            node_backup = NodeBackup}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
project_loaded(?clean_build, S = #state{prj_cfg = PC}) ->
    dispatch_errors(
      repoxy_project_rebar:rebar(
        PC#prj_cfg.rebar_cfg,
        ['clean', 'get-deps', 'compile', 'repoxy_discover'])),
    {next_state, project_loaded, S};

project_loaded(?app_found(AI), S = #state{prj_cfg = PC}) ->
    dispatch_on_load(AI, store_app_info(load_app(unload_app(AI, S))))
    case repoxy_project_code:load_app_into_node(AI) of
        {ok, _} ->
            NewPC = add_app_info(AI, PC),
            repoxy_project_events:notify(?on_app_load(AI));
        Err ->
            dispatch_errors(Err)
    end,
    {next_state, project_loaded, S};

project_loaded(?unload,
               #state{node_backup = NodeBackup,
                      prj_cfg = #prj_cfg{build_dir = BuildDir}}) ->
    unload(NodeBackup, BuildDir),
    {next_state, no_project_loaded,
     #state{prj_cfg = no_project_loaded,
            node_backup = no_node_backup}};

project_loaded(Other, S) ->
    dispatch_errors({error, {unexpected_event, Other}}),
    {next_state, project_loaded, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, Prj_Cfg) ->
    {next_state, StateName, Prj_Cfg}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, Prj_Cfg) ->
    Reply = ok,
    {reply, Reply, StateName, Prj_Cfg}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, StateName, Prj_Cfg) ->
    {next_state, StateName, Prj_Cfg}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_, no_project_loaded, _State) -> ok;
terminate(_, project_loaded, #state{node_backup = NodeBackup,
                                    prj_cfg = #prj_cfg{build_dir = BuildDir}}) ->
    unload(NodeBackup, BuildDir).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, Prj_Cfg, _Extra) ->
    {ok, StateName, Prj_Cfg}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unload(NodeBackup, BuildDir) ->
    repoxy_project_code:restore_node(NodeBackup),
    repoxy_project_code:clean_build_dir(BuildDir),
    repoxy_project_events:notify(?on_unload).

dispatch_errors(ok) ->
    ok;
dispatch_errors({ok, V}) ->
    {ok, V};
dispatch_errors(Err) ->
    repoxy_project_events:notify(?on_internal_error(Err)).

%%------------------------------------------------------------------------------
%% @doc
%% Insert an application info record into the a project config, potentially
%% overwriting an existing configuration.
%% @end
%%------------------------------------------------------------------------------
add_app_info(AppInfo, Cfg) ->
    #app_info{name = AppName} = AppInfo,
    #prj_cfg{app_infos = AppInfos} = Cfg,
    NewAppInfos = lists:keystore(AppName, ?APP_NAME_POS, AppInfos, AppInfo),
    Cfg#prj_cfg{app_infos = NewAppInfos}.


unload_app(

    dispatch_on_load(AI, store_app_info(load_app(unload_app(AI, S))))
    case repoxy_project_code:load_app_into_node(AI) of
        {ok, _} ->
            NewPC = add_app_info(AI, PC),
            repoxy_project_events:notify(?on_app_load(AI));
        Err ->
            dispatch_errors(Err)
    end,
