%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Contains the main functions for the escriptized version as well as
%%% an OTP application callback.
%%% @end
%%% Created : 12 Dec 2012 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(repoxy).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([main/1, shutdown/0]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
main(Args) ->
    io:format(<<"

 -.- REPOXY -.-

The Epoxy Coat for Rebar - enables interactive
erlang building and unit test execution.

Usage: invoke repoxy from the directory where you would
invoke rebar, i.e. the projects base directory.

Starts a tcp server accepting simple messages containing
s-expressions.

">>),
    error_logger:info_msg("Running in directory \"~p\".~n",
                          [file:get_cwd()]),

    process_flag(trap_exit, true),
    start_apps(Args),
    wait_for_stop(),
    stop_apps().

%%--------------------------------------------------------------------
%% @doc
%% Exits repoxy, Can be called from anywhere to exit.
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    shutdown_proc ! kill_pill.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_apps(Args) ->
    application:start(sasl),
    application:start(smooth),
    application:start(nano_trace),
    application:load(repoxy),
    case Args of
        [PortStr|_] ->
            Port = list_to_integer(PortStr),
            application:set_env(repoxy, tcp_port, Port);
        _ ->
            ok
    end,
    application:start(repoxy, permanent),
    nano_trace:start([repoxy, rebar], "/tmp/repoxy_trace.log"),
    nano_trace:msg_depth(100).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
stop_apps() ->
    nano_trace:stop(),
    application:stop(repoxy),
    application:start(nano_trace),
    application:stop(smooth),
    application:stop(sasl).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
wait_for_stop() ->
    register(shutdown_proc, self()),
    await_kill_pill().

await_kill_pill() ->
    receive
        kill_pill ->
            error_logger:info_msg("*** KILL PILL RECEIVED ***~n~nShutting down.~n");
        Msg ->
            error_logger:info_msg("Oh, this wasn't a kill pill: ~w~n", [Msg]),
            await_kill_pill()
    end.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start(_StartType, _StartArg) ->
    repoxy_project_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
