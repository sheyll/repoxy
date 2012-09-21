%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% The actual core functionality that repoxy provides.
%%% @end
%%% Created : 16 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_core).

-export([empty_prj_cfg/0,
         load_rebar/0,
         backup_node/0,
         restore_node/1,
         rebar/2,
         add_app_info/2,
         get_app_infos/1,
         load_apps_into_node/1]).

-export_type([project_cfg/0,
              node_backup/0]).

-include("repoxy_core.hrl").

-record(project_cfg, {rebar_cfg = no_rebar_cfg
                      :: rebar_config:config(),
                      app_infos = []
                      :: [#app_info{}]}).

-type project_cfg() :: #project_cfg{}.

-record(node_backup, {apps_loaded,
                      code_path,
                      loaded_modules}).

-type node_backup() :: #node_backup{}.

-include_lib("repoxy/include/state_m.hrl").


%%------------------------------------------------------------------------------
%% @doc
%% Create an empty project_cfg - use this only in unit tests.
%% @end
%%------------------------------------------------------------------------------
-spec empty_prj_cfg() ->
                           project_cfg().
empty_prj_cfg() ->
    #project_cfg{}.

%%------------------------------------------------------------------------------
%% @doc
%% Start rebar the first time, initialize the configuration and do all necessary
%% setup work. The return value must be passed into all subsequent calls.
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar() ->
                        project_cfg().
load_rebar() ->
    error_logger:info_msg("Starting rebar ...~n"),
    ok = application:load(rebar),
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,
    Config = create_config(),
    start_logging(Config),
    error_logger:info_msg("...done.~n"),
    #project_cfg{rebar_cfg = Config, app_infos = []}.

%%------------------------------------------------------------------------------
%% @doc
%% Save code_path and loaded applications to be restored with `restore_node'.
%% @end
%%------------------------------------------------------------------------------
-spec backup_node() ->
                         node_backup().
backup_node() ->
    #node_backup{apps_loaded = application:loaded_applications(),
                 code_path = code:get_path(),
                 loaded_modules = code:all_loaded()}.

%%------------------------------------------------------------------------------
%% @doc
%% Unload the rebar application from the current node. Also reset the code_path
%% and stop/unload all applications that were not loaded before.
%% @end
%%------------------------------------------------------------------------------
-spec restore_node(node_backup()) ->
                          ok.
restore_node(#node_backup{apps_loaded = OldAppsLoaded,
                          code_path = OldCodePath,
                          loaded_modules = OldModules}) ->
    error_logger:info_msg("~n  *** RESETTING NODE ***~n"),
    [try
         AppName = element(1, A),
         error_logger:info_msg("Unloading ~p~n", [AppName]),
         application:stop(AppName),
         application:unload(AppName)
     catch
         C:E ->
             error_logger:info_msg("Unloading ~p failed: ~p:~p ~n", [A, C, E])
     end
     || A <- application:loaded_applications(),
        not lists:member(A, OldAppsLoaded)],

    error_logger:info_msg("Resetting code_path to ~p~n", [OldCodePath]),
    code:set_path(OldCodePath),

    [begin
         case string:str(atom_to_list(Mod), "repoxy") of
             0 ->
                 error_logger:info_msg("Purging ~p~n", [Mod]),
                 code:delete(Mod),
                 code:purge(Mod);
             _ ->
                 error_logger:info_msg("NOT Purging ~p~n", [Mod])
         end
     end
     || M = {Mod, _} <- code:all_loaded(),
        not lists:member(M, OldModules)],

    error_logger:info_msg("Successfully resetted node.~n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Run rebar command.
%% @end
%%------------------------------------------------------------------------------
-spec rebar(project_cfg(), [atom()] | atom()) ->
                   ok | {error, TextualOutput :: string()}.
rebar(Cfg, RebarCmds) ->
    RebarCmds1 = if
                     is_list(RebarCmds) ->
                         RebarCmds;
                     is_atom(RebarCmds) ->
                         [RebarCmds]
                 end,
    try
        rebar_core:process_commands(RebarCmds1, Cfg#project_cfg.rebar_cfg),
        ok
    catch
        throw:rebar_abort ->
            {error, {{"Rebar command failed."}, RebarCmds}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Add an app info to the
%% @end
%%------------------------------------------------------------------------------
-spec add_app_info(#app_info{}, project_cfg()) ->
                          ok | {error, TextualOutput :: string()}.
add_app_info(AppInfo, Cfg) ->
    #app_info{name = AppName} = AppInfo,
    #project_cfg{app_infos = AppInfos} = Cfg,
    NewAppInfos = lists:keystore(AppName, ?APP_NAME_POS, AppInfos, AppInfo),
    Cfg#project_cfg{app_infos = NewAppInfos}.

%%------------------------------------------------------------------------------
%% @doc
%% Extract #app_info{}'s from a project config. Useful for unit tests.
%% @return all app_info records.
%% @end
%%------------------------------------------------------------------------------
-spec get_app_infos(project_cfg()) ->
                          [#app_info{}].
get_app_infos(Cfg) ->
    Cfg#project_cfg.app_infos.

%%------------------------------------------------------------------------------
%% @doc
%% Load the applications added by `add_app_info/2' into the current node
%% in order to enable compilation and execution of the modules in of an app.
%% The effect of this can only be reversed by `restore_node/1'.
%% @end
%%------------------------------------------------------------------------------
-spec load_apps_into_node(project_cfg()) ->
                                 [#app_info{}].
load_apps_into_node(Cfg) ->
    [load_app_into_node(AppCfg) ||
        AppCfg <- Cfg#project_cfg.app_infos].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_app_into_node(#app_info{
                      name = Name,
                      lib_paths = LibPaths
                     }) ->
    error_logger:info_msg("Loading ~p~n.", [Name]),
    error_logger:info_msg("Adding paths: ~p~n.", [code:add_pathsa(LibPaths)]),
    Loaded = application:load(Name),
    error_logger:info_msg("Loading application: ~p~n.", [Loaded]),
    error_logger:info_msg("Done loading ~p~n~n.", [Name]),
    {Name, Loaded}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_config() ->
    ?do(repoxy_rebar_cfg:new(),
        [
         fun repoxy_rebar_cfg:replace_with_global_config/1,
         fun repoxy_rebar_cfg:add_operation_counter/1,
         fun repoxy_rebar_cfg:add_vsn_cache/1,
         fun repoxy_rebar_cfg:add_log_level/1,
         fun repoxy_rebar_cfg:add_script_name/1,
         fun repoxy_rebar_cfg:load_project_config/1,
         fun repoxy_rebar_cfg:add_proj_dir/1,
         fun repoxy_rebar_cfg:add_repoxy_plugin/1
        ]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_logging(Cfg) ->
    ok = rebar_log:init(Cfg).
