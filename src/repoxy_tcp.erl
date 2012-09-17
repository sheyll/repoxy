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

-record(state, {lsock, collected_data}).

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
    error_logger:info_msg("Waiting for client.~n"),
    {ok, ClientSock} = gen_tcp:accept(State#state.lsock),
    inet:setopts(ClientSock,[{active,once}]),
    error_logger:info_msg("Connection to client established.~n"),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
connected({tcp_closed, _Sock}, State) ->
    error_logger:info_msg("Client disconnected.~n"),
    accepting(accept, State);
connected({tcp, CSock, Data}, State) ->
    log_result(
      CSock,
      Data,
      send_response(
        CSock,
        handle_request(
          store_unfinished(
            State,
            parse_input(
              State#state.collected_data, Data))))),
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
parse_input(CollectedData, []) ->
    case repoxy_sexp:to_erl(CollectedData) of
        {ok, Term} ->
            {{ok, Term}, []};
        _Err ->
            {unfinished, CollectedData}
    end;

parse_input(CollectedData, [NewChar | NewRest]) ->
    InMsg = CollectedData ++ [NewChar],
    case repoxy_sexp:to_erl(InMsg) of
        {ok, Term} ->
            {{ok, Term}, NewRest};
        _Err ->
            parse_input(InMsg, NewRest)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
store_unfinished(State, {Res, ToCollect}) ->
    {Res, State#state{collected_data = ToCollect}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_request({{ok, Req}, State}) ->
    {reply, repoxy_facade:handle_request(Req), State};
handle_request({_Err, State}) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
send_response(CSock, Res = {reply, OutTerm, State}) ->
    OutMsg = repoxy_sexp:from_erl(Term),
    gen_tcp:send(CSock, OutMsg ++ "\n"),
    Res;
send_response(_CSock, Res = {noreply, State}) ->
    Res.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
log_result(CSock, InData, Req) ->
    LogMessage = lists:flatten(
                   io_lib:format("Processed request ~s from ~w: ~p~n",
                                 [InData, CSock, Req])),
    error_logger:info_msg(LogMessage).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
server_loop(CSock, State) ->
    inet:setopts(CSock, [{active,once}]),
    {next_state, connected, State}.
