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
         recompile/1]).

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
    load_app(),
    Config = create_config(),
    start_logging(Config),
    error_logger:info_msg("...done.~n"),
    Config.

%%------------------------------------------------------------------------------
%% @doc
%% Recompile the whole project using rebar.
%% @end
%%------------------------------------------------------------------------------
-spec recompile(repoxy_rebar_cfg:cfg()) ->
                       ok | {error, TextualOutput :: string()}.
recompile(Cfg) ->
    try
        rebar_core:process_commands([compile], Cfg),
        ok
    catch
        throw:rebar_abort ->
            {error, "Compilation failed."}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_app() ->
    ok = application:load(rebar),
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end.

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
         fun repoxy_rebar_cfg:add_proj_dir/1
        ]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_logging(Cfg) ->
    ok = rebar_log:init(Cfg).
