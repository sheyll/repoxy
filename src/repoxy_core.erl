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
         get_app_paths/1,
         compile_file/2,
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

-define(REPOXY_EBIN_DIR, ".repoxy_ebin").

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
    error_logger:info_msg("Cleaning repoxy eunit directory: ~p~n",
                          [file:del_dir(?REPOXY_EBIN_DIR)]),
    filelib:ensure_dir(filename:join([?REPOXY_EBIN_DIR, "dummy.beam"])),
    %% make sure that the repoxy bin dir is always first
    code:add_patha(?REPOXY_EBIN_DIR),
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
        rebar_core:process_commands(RebarCmds1, Cfg#project_cfg.rebar_cfg),
        ok
    catch
        C:E ->
            {error, {{rebar_failed, RebarCmds}, C, E}}
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
%% Return a list of app name - app base directory pairs for each loaded app.
%% These tuples represent a reduced `#app_info' record.
%% @end
%%------------------------------------------------------------------------------
-spec get_app_paths(project_cfg()) ->
                           [{AppName :: atom(), AppBaseDir :: string()}].
get_app_paths(#project_cfg{app_infos = AppInfos}) ->
    [{AppName, AppBaseDir}
     || #app_info{name = AppName, cwd = AppBaseDir} <- AppInfos].

%%------------------------------------------------------------------------------
%% @doc
%% Compile a single file and return the compilation results. Files are compiled
%% into a special directory.
%% @end
%%------------------------------------------------------------------------------
-spec compile_file(project_cfg(), string()) ->
                                 term().
compile_file(Cfg, File) ->
    compile:file(
      File,
      add_repoxy_erl_opts(
        lookup_app_erl_opts(File, Cfg))).

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
         fun repoxy_rebar_cfg:add_repoxy_plugin/1,
         fun repoxy_rebar_cfg:add_keep_going/1
        ]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_logging(Cfg) ->
    ok = rebar_log:init(Cfg).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_repoxy_erl_opts(Opts) ->
    [debug_info,
     verbose,
     report,
     {outdir,?REPOXY_EBIN_DIR}] ++ Opts.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
lookup_app_erl_opts(File, #project_cfg{app_infos =  AIs}) ->
    case get_app_info_for_file(File, AIs) of
        {ok, #app_info{erl_opts = EOs}} ->
            EOs;
        _ ->
            []
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec get_app_info_for_file(string(), [#app_info{}]) ->
                                   {ok, #app_info{}} | error.
get_app_info_for_file(File, AppInfos) ->
    AbsFile = filename:absname(File),
    case
        [AI || AI = #app_info{cwd = AD} <- AppInfos,
               string:str(AbsFile, AD) =:= 1]
    of
        [A] ->
            {ok, A};
        _ ->
            error
    end.
