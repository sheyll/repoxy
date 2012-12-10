%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc

%%% Dispatches the events that occur when the state of a project changes, for
%%% example, when after compilation new compiler errors are found.

%%% @end
%%% Created :  8 Dec 2012 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(repoxy_project_events).

%% API
-export([start_link/0, add_sup_handler/2, notify/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the event manager.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%%--------------------------------------------------------------------
%% @doc
%% Attach a handler to the project events manager.
%% @see gen_event:add_sup_handler/3
%% @end
%%--------------------------------------------------------------------
add_sup_handler(Handler, Args) ->
    gen_event:add_sup_handler(?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Send a notification to all handlers.
%% @see gen_event:notify/2
%% @end
%%--------------------------------------------------------------------
notify(Msg) ->
    gen_event:notify(?MODULE, Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
