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

-behaviour(repoxy_evt).
-behaviour(gen_fsm).

%% API
-export([start_link/1, on_project_event/2]).

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
%% Start a tcp server on localhost:5678
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Port, []).

%%%===================================================================
%%% repoxy_evt callbacks
%%%===================================================================

%% @private
on_project_event(Pid, ProjectEvent) ->
    gen_fsm:send_all_state_event(Pid, {project_event, ProjectEvent}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-record(state, {lsock, csock, collected_data}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init(Port) ->
    repoxy_evt:add_sup_handler(?MODULE, self()),
    gen_fsm:send_event(self(), accept),
    {ok, LSock} = gen_tcp:listen(Port, [{packet, raw},
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
    error_logger:info_msg("Client disconnected.~n"),
    accepting(accept, State);
connected({tcp, CSock, Data}, State) ->
    process_incoming_data(Data, State#state{csock = CSock}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
need_more_data({tcp_closed, Sock}, State) ->
    error_logger:error_msg(
      "Client ~p disconnected before completing request: ~s.~n",
      [Sock, State#state.collected_data]),
    accepting(accept, State#state{collected_data=[],
                                  csock=no_socket});
need_more_data({tcp, CSock, Data}, State) ->
    process_incoming_data(Data, State#state{csock = CSock}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_event(_Event, accepting, State) ->
    {next_state, accepting, State};
handle_event({project_event, ProjectEvent}, StateName, State) ->
    send_event(ProjectEvent, State),
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
      process_request(
        try_to_parse(
          concat_collected_and_new(Data, State)))).

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
    error_logger:info_msg("Closing after completing request."),
    ok = gen_tcp:close(State#state.csock),
    accepting(accept, State#state{csock = no_socket});
until_request_complete({{ok, _Req}, _Reply, State}) ->
    {next_state, connected, State#state{collected_data = []}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
process_request({{ok, [close]}, State}) ->
    {{ok, [close]}, ignore, State};
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
concat_collected_and_new(Data, State = #state{collected_data = []}) ->
    State#state{collected_data = string:strip(Data, left)};
concat_collected_and_new(Data, State) ->
    State#state{collected_data = State#state.collected_data ++ Data}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
send_event(Event, State) ->
    OutMsg = repoxy_sexp:from_erl(repoxy_facade:format_event(Event)),
    gen_tcp:send(State#state.csock, OutMsg).
