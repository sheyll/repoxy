%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% The actual core functionality that repoxy provides.
%%% @end
%%% Created : 16 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_core).

-export([load_rebar/0,
         backup_node/0,
         restore_node/1,
         rebar/2]).

-export_type([project_cfg/0,
              node_backup/0]).

-include("repoxy_core.hrl").

-type app_info() :: #app_info{}.

-type app_infos() :: [{module(), #app_info{}}].

-record(project_cfg, {rebar_cfg,
                      app_infos :: app_infos()}).
-type project_cfg() :: #project_cfg{}.

-record(node_backup, {apps_loaded,
                      code_path,
                      loaded_modules}).

-type node_backup() :: #node_backup{}.

-include_lib("repoxy/include/state_m.hrl").


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
