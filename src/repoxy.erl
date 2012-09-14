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
main([RebarFile]) ->
    process_flag(trap_exit, true),
    application:start(sasl),
    application:start(repoxy),
    repoxy_sup:start_link(RebarFile),
    wait_for_stop(),
    application:stop(repoxy);
main(_) ->
    io:format(<<"

 -.- REPOXY -.-

The Epoxy Coat for Rebar - enables an emacs mode for improved
erlang programming.

Erlang node that compiles sources of a rebar project and runs tests.
Used best with emacs and the corresponding mode.

Usage: ./repoxy <path-to-rebar.config>

Starts the server on port 5678.

">>).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
shutdown() ->
    shutdown_proc ! stop.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
wait_for_stop() ->
    register(shutdown_proc, self()),
    receive
        _KillPill ->
            error_logger:info_msg("Shutting down...")
    end.
