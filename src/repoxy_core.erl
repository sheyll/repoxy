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
         unload_rebar/0,
         rebar/2,
         load_apps/1]).

-include_lib("repoxy/include/state_m.hrl").


%%------------------------------------------------------------------------------
%% @doc
%% Start rebar the first time, initialize the configuration and do all necessary
%% setup work. The return value must be passed into all subsequent calls.
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar() ->
                        repoxy_rebar_cfg:cfg().
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
    Config.

%%------------------------------------------------------------------------------
%% @doc
%% Unload the rebar application from the current node.
%% @end
%%------------------------------------------------------------------------------
-spec unload_rebar() ->
                          ok.
unload_rebar() ->
    error_logger:info_msg("Unloading rebar ...~n"),
    application:stop(rebar),
    application:unload(rebar),
    error_logger:info_msg("...done.~n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Run rebar command.
%% @end
%%------------------------------------------------------------------------------
-spec rebar(repoxy_rebar_cfg:cfg(), [atom()] | atom()) ->
                   ok | {error, TextualOutput :: string()}.
rebar(Cfg, RebarCmds) ->
    RebarCmds1 = if
                     is_list(RebarCmds) ->
                         RebarCmds;
                     is_atom(RebarCmds) ->
                         [RebarCmds]
                 end,
    try
        rebar_core:process_commands(RebarCmds1, Cfg),
        ok
    catch
        throw:rebar_abort ->
            {error, {{"Rebar command failed."}, RebarCmds}}
    end.


%%------------------------------------------------------------------------------
%% @doc
%% Load all applications of the current project.
%% @end
%%------------------------------------------------------------------------------
-spec load_apps(repoxy_rebar_cfg:cfg()) ->
                       ok | {error, TextualOutput :: string()}.
load_apps(Cfg) ->
    %% add rebar lib_dir
    %% add rebar deps
    %% add all subdirs
    ok.

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
