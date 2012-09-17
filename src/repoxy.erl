%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Provide main/1 function to enable 'rebar escriptize'
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy).

-export([main/1, shutdown/0]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
main([]) ->
    io:format(<<"

 -.- REPOXY -.-

The Epoxy Coat for Rebar - enables interactive
erlang building and unit test execution.

Usage: invoke repoxy from the directory where you would
invoke rebar, i.e. the projects base directory.

Starts a server on port 5678, that accpets s-expressions.

">>),
    process_flag(trap_exit, true),
    start_apps(),
    repoxy_sup:start_link(),
    wait_for_stop(),
    stop_apps().

%%--------------------------------------------------------------------
%% @doc
%% Exits repoxy, Can be called from anywhere to exit.
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    shutdown_proc ! stop.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_apps() ->
    application:start(sasl),
    application:start(repoxy, permanent).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
stop_apps() ->
    application:stop(repoxy),
    application:stop(sasl).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
wait_for_stop() ->
    register(shutdown_proc, self()),
    receive
        _KillPill ->
            error_logger:info_msg("Shutting down...")
    end.
