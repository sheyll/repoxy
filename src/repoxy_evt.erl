%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc

%%% Dispatches the events that occur when the state of a project changes, for
%%% example, when after compilation new compiler errors are found.

%%% @end
%%% Created :  8 Dec 2012 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(repoxy_evt).

%% API
-export([start_link/0, add_sup_handler/2, notify/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(client, {mod,arg}).

%%%===================================================================
%%% API
%%%===================================================================

-callback on_project_event(term(), term()) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Start the event manager.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%%--------------------------------------------------------------------
%% @doc
%% Attach a handler to the project events manager.  The function
%% `Module:on_project_event(Arg, Event)' is called for each event.
%% @end
%%--------------------------------------------------------------------
add_sup_handler(Module, Arg) ->
    gen_event:add_sup_handler(?MODULE, ?MODULE,
                              #client{mod=Module, arg=Arg}).

%%--------------------------------------------------------------------
%% @doc
%% Send a notification to all handlers.
%% @see gen_event:notify/2
%% @end
%%--------------------------------------------------------------------
notify(Msg) ->
    gen_event:notify(?MODULE, Msg).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Client) ->
    {ok, Client}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_event(Event, State = #client{mod=M, arg=A}) ->
    M:on_project_event(A, Event),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
