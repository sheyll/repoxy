%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Compiles files in a rebar project, and communicates via S-Expressions
%%% via a pipe with an external programm.
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
    inet:setopts(CSock, [{active,once}]),
    ScanRes = simple_sexp_scanner:string(Data),
    case ScanRes of
        {ok, Tokens, _} ->
            Request = simple_sexp_parser:parse(Tokens),
            LogMsg = lists:flatten(io_lib:format("~p", [Request])),
            error_logger:info_msg("Received request  " ++ LogMsg);
        Error ->
            LogMsg = lists:flatten(io_lib:format("~p", [Error])),
            error_logger:error_msg("Scan Error  " ++ LogMsg)
    end,
    {next_state, connected, State}.

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
