%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% The server and gen_fsm callbacks for {@link repoxy_project}.
%%% @end
%%% Created :  8 Dec 2012 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(repoxy_project_server).

-behaviour(repoxy_evt).
-behaviour(gen_fsm).

%% event handler callback
-export([on_project_event/2]).

%% gen_fsm callbacks
-export([init/1,
         no_project_loaded/2,
         project_loaded/2,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-include("../include/repoxy.hrl").
-include("repoxy_project_server.hrl").

-record(state, {node_backup = no_node_backup
                :: #node_backup{} | no_node_backup,
                prj_cfg     = no_prj_cfg
                :: #prj_cfg{} | no_prj_cfg}).

%%%===================================================================
%%% repoxy_evt callbacks
%%%===================================================================

%% @private
on_project_event(Pid, E = ?on_app_discovered(_AppInfo)) ->
    gen_fsm:send_event(Pid, E);
on_project_event(_Pid, _) ->
    ok.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% @private
init([]) ->
    process_flag(trap_exit, true),
    repoxy_evt:add_sup_handler(?MODULE, self()),
    {ok, no_project_loaded, #state{}}.

%% @private
no_project_loaded(?load(Dir), _State) ->
    {NewStateName, NewState} = load_project(Dir),
    {next_state, NewStateName, NewState}.

%% @private
project_loaded(?clean_build, S = #state{prj_cfg = PC}) ->
    dispatch_errors(repoxy_rebar:discover(PC#prj_cfg.rebar_cfg)),
    {next_state, project_loaded, S};

project_loaded(?on_app_discovered(AI), S) ->
    NewS = load_app(AI, unload_app(AI, S)),
    {next_state, project_loaded, NewS};

project_loaded(?unload, State) ->
    {next_state, no_project_loaded, unload_project(State)};

project_loaded(Other, S) ->
    dispatch_errors({error, {unexpected_event, Other}}),
    {next_state, project_loaded, S}.

%% @private
handle_event(_Event, StateName, Prj_Cfg) ->
    {next_state, StateName, Prj_Cfg}.

%% @private
handle_sync_event(_Event, _From, StateName, Prj_Cfg) ->
    Reply = ok,
    {reply, Reply, StateName, Prj_Cfg}.

%% @private
handle_info(_Info, StateName, Prj_Cfg) ->
    {next_state, StateName, Prj_Cfg}.

%% @private
terminate(_, no_project_loaded, _State) -> ok;
terminate(_, project_loaded, State) ->
    unload_project(State),
    ok.

%% @private
code_change(_OldVsn, StateName, Prj_Cfg, _Extra) ->
    {ok, StateName, Prj_Cfg}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Load a project. Creates a backup of current erl-vm state, build dirs,
%% etc...
load_project(Dir) ->
    try begin
            BuildDir = repoxy_build_dir:new_build_dir(),
            NodeBackup = repoxy_node_backup:backup_node(),
            RebarCfg = repoxy_rebar:load_rebar(Dir),
            PC = #prj_cfg{build_dir = BuildDir, rebar_cfg = RebarCfg},
            repoxy_evt:notify(?on_project_load(PC)),
            #state{prj_cfg = PC, node_backup = NodeBackup}
        end
    of
        State ->
            {project_loaded, State}
    catch
        C:Exception ->
            repoxy_evt:notify(
              ?on_project_load_failed(Dir, {C, Exception})),
            {no_project_loaded, #state{}}
    end.

%% @doc Unload the whole project by resetting the whole erlang node into its
%% pre-load state.
unload_project(#state{node_backup = NodeBackup,
                      prj_cfg = PC = #prj_cfg{build_dir = BuildDir}}) ->
    dispatch_errors(repoxy_node_backup:restore_node(NodeBackup)),
    dispatch_errors(repoxy_build_dir:clean_build_dir(BuildDir)),
    repoxy_evt:notify(?on_project_unload(PC)),
    #state{}.

%% @private
dispatch_errors(ok) ->
    ok;
dispatch_errors({ok, V}) ->
    {ok, V};
dispatch_errors(Err) ->
    repoxy_evt:notify(?on_internal_error(Err)),
    Err.

%% @doc Load an application found by {@link repoxy_rebar_plugin}, and all
%% modules of that application, add the libdir and dispatch the corresponding
%% event: `?on_app_load(AppInfo)'
load_app(AI, S) ->
    case repoxy_apps:load_app(AI) of
        {error, _} ->
            S;
        _Ok ->
            add_app_build_cfg(AI, S)
    end.

%% @private
unload_app(AI = #app_build_cfg{name = AppName}, S) ->
    case lookup_appinfo(AppName, S) of
        false ->
            S;
        _AI ->
            repoxy_apps:unload_app(AI),
            remove_app_build_cfg(AppName, S)
    end.

%% @private
lookup_appinfo(AppName, #state{prj_cfg = #prj_cfg{app_build_cfgs = AppInfos}}) ->
    lists:keyfind(AppName, ?APP_NAME_POS, AppInfos).

%% @private
add_app_build_cfg(AppInfo, S=#state{prj_cfg = Cfg}) ->
    #app_build_cfg{name = AppName} = AppInfo,
    #prj_cfg{app_build_cfgs = AppInfos} = Cfg,
    NewAppInfos = lists:keystore(AppName, ?APP_NAME_POS, AppInfos, AppInfo),
    S#state{prj_cfg = Cfg#prj_cfg{app_build_cfgs = NewAppInfos}}.

%% @private
remove_app_build_cfg(AppName, S=#state{prj_cfg = Cfg}) ->
    #prj_cfg{app_build_cfgs = AppInfos} = Cfg,
    NewAppInfos = lists:keydelete(AppName, ?APP_NAME_POS, AppInfos),
    S#state{prj_cfg = Cfg#prj_cfg{app_build_cfgs = NewAppInfos}}.

%%
%% TODO define 'test' with reference to real code that it calls
%% TODO discover App tests
%% TODO discover syntax errors
%% TODO peer_remote
%% TODO run tests
%% TODO trace tests
%% TODO code formatting
%% TODO coverage
%% TODO xref (async)
%% TODO dialyzer (async)
%% TODO load module
%% TODO check syntax of module
%% TODO debug tests??
%%
%% TODO live infos in extra menu in emacs this contains all the 'live'
%%      data (module is loaded, xref results, dialyzer results, debug??, trace??)
%%
