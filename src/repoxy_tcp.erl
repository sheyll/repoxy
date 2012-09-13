%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% A tcp server that listens on port 5678 for communication with
%%% repoxy.
%%% @end
%%% Created : 10 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_tcp).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, accepting/2, connected/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-record(state, {lsock}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
    gen_fsm:send_event(self(), accept),
    {ok, LSock} = gen_tcp:listen(5678, [{packet, line},
                                        {exit_on_close, true},
                                        {reuseaddr, true},
                                        {active, false}]),
    {ok, accepting, #state{lsock = LSock}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
accepting(accept, State) ->
    error_logger:info_msg("Waiting for client."),
    {ok, ClientSock} = gen_tcp:accept(State#state.lsock),
    inet:setopts(ClientSock,[{active,once}]),
    error_logger:info_msg("Connection to client established."),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
connected({tcp_closed, _Sock}, State) ->
    error_logger:info_msg("Client disconnected."),
    accepting(accept, State);
connected({tcp, CSock, Data}, State) ->
    log_result(
      CSock,
      Data,
      send_response(
        CSock,
        handle_request(
          parse_input(Data)))),
    server_loop(State, CSock).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
log_result(CSock, InData, {ok, _}) ->
    LogMessage = lists:flatten(
                   io_lib:format("Successfully processed request from ~w~n",
                                 [CSock])),
    error_logger:info_msg(LogMessage);
log_result(CSock, InData, Err) ->
    LogMessage = lists:flatten(
                   io_lib:format("Error processing request ~s from ~w: ~p~n",
                                 [InData, CSock, Err])),
    error_logger:info_msg(LogMessage).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
server_loop(State, CSock) ->
    inet:setopts(CSock, [{active,once}]),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
send_response(CSock, Term) ->
    {ok, OutMsg} = repoxy_sexp:from_erl(Term),
    gen_tcp:send(CSock, OutMsg ++ "\n"),
    Term.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
parse_input(Data) ->
    InMsg = string:strip(Data, both, $\n),
    repoxy_sexp:to_erl(InMsg).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_request({ok, Req}) ->
    repoxy_facade:handle_request(Req);
handle_request(Err) ->
    Err.
