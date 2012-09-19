%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% A tcp server that listens on port 5678 for clients that communicate
%%% with repoxy via s-expressions.
%%% @end
%%% Created : 10 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_tcp).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,

         accepting/2, connected/2, need_more_data/2,

         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

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

-record(state, {lsock, csock, collected_data}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
    gen_fsm:send_event(self(), accept),
    {ok, LSock} = gen_tcp:listen(5678, [{packet, raw},
                                        {mode, list},
                                        {exit_on_close, true},
                                        {reuseaddr, true},
                                        {nodelay, true},
                                        {active, false}]),
    {ok, accepting, #state{lsock = LSock}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
accepting(accept, State) ->
    error_logger:info_msg("Waiting for client.~n"),
    {ok, ClientSock} = gen_tcp:accept(State#state.lsock),
    error_logger:info_msg("Connection to client established.~n"),

    NewState = State#state{csock = ClientSock, collected_data = []},
    reactivate_socket(NewState),
    {next_state, connected, NewState}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
connected({tcp_closed, _Sock}, State) ->
    repoxy_facade:handle_request([reset]),
    error_logger:info_msg("Client disconnected.~n"),
    accepting(accept, State);
connected({tcp, CSock, Data}, State) ->
    process_incoming_data(Data, State#state{csock = CSock}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
need_more_data({tcp_closed, Sock}, State) ->
    repoxy_facade:handle_request([reset]),
    error_logger:error_msg(
      "Client ~p disconnected before completing request: ~s.~n",
      [Sock, State#state.collected_data]),
    accepting(accept, State);
need_more_data({tcp, CSock, Data}, State) ->
    process_incoming_data(Data, State#state{csock = CSock}).

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

process_incoming_data(Data, State) ->
    reactivate_socket(State),
    until_request_complete(
      log_result(
        send_response(
          process_request(
            try_to_parse(
              concat_collected_and_new(Data, State)))))).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
reactivate_socket(State) ->
    inet:setopts(State#state.csock, [{active,once}]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
until_request_complete({_Incomplete, State}) ->
    {next_state, need_more_data, State};
until_request_complete({{ok, [close]}, _, State}) ->
    accepting(accept, State);
until_request_complete({{ok, _Req}, _Reply, State}) ->
    {next_state, connected, State#state{collected_data = []}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
log_result(In = {{ok, Req}, Reply, State}) ->
    LogMsg = lists:flatten(
               io_lib:format(
                 "Processed request ~w from ~w with result ~w~n",
                 [Req, State#state.csock, Reply])),
    error_logger:info_msg(LogMsg),
    In;
log_result(In = {_Err, State}) ->
    LogMsg = lists:flatten(
               io_lib:format(
                 "Incomplete request \"~s\" from ~w~n",
                 [State#state.collected_data, State#state.csock])),
    error_logger:info_msg(LogMsg),
    In.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
send_response(Complete = {{ok, [close]}, _, State}) ->
    gen_tcp:close(State#state.csock),
    Complete;
send_response(Complete = {{ok, _Req}, Reply, State}) ->
    OutMsg = repoxy_sexp:from_erl(Reply),
    gen_tcp:send(State#state.csock, OutMsg),
    Complete;
send_response(Incomplete) ->
    Incomplete.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
process_request({{ok, [close]}, State}) ->
    Reply = repoxy_facade:handle_request([reset]),
    {{ok, [close]}, Reply, State};
process_request({{ok, Req}, State}) ->
    Reply = repoxy_facade:handle_request(Req),
    {{ok, Req}, Reply, State};
process_request(Incomplete) ->
    Incomplete.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
try_to_parse(State) ->
    case repoxy_sexp:to_erl(State#state.collected_data) of
        {ok, Term} ->
            {{ok, Term}, State};
        _Err ->
            {unfinished, State}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
concat_collected_and_new(Data, State) ->
    State#state{collected_data = State#state.collected_data ++ Data}.
