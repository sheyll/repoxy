%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Process incoming requests coming in via `handle_request' in order
%%% to provide the functionality used by clients.
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade).

-behaviour(gen_fsm).

%% API
-export([handle_request/1,
         start_link/0]).

%% gen_fsm callbacks
-export([init/1,
         not_loaded/3,
         project_loaded/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the facade fsm that handles request for a single rebar
%% project. Currently only one concurrent project and client can
%% exist.
%% @parameter a string containing the absolute path to a rebar config.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Handle incoming erlang terms that represent a command/task/request.
%% @return an erlang term, that can be converted to an s-expression,
%%         that is supposed to be sent to the outside world.
%% @end
%%--------------------------------------------------------------------
-spec handle_request(repoxy_sexp:sexp_term()) ->
                            {ok, repoxy_sexp:sexp_term()} |
                            {error, repoxy_sexp:sexp_term()}.
handle_request(Req) ->
    gen_fsm:sync_send_event(?SERVER, Req, infinity).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
    {ok, project_loaded, repoxy_core:load_rebar()}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
not_loaded(close, _From, State) ->
    {reply, closed, not_loaded, State};
not_loaded(load, _From, _State) ->
    {reply, ok, project_loaded, repoxy_core:load_rebar()}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
project_loaded(close, _From, State) ->
    repoxy_core:unload_rebar(),
    {reply, closed, not_loaded, no_rebar_cfg};
project_loaded(Request, _From, State) ->
    case Request of
        [Function|Args] ->
            case
                erlang:function_exported(
                  repoxy_core,
                  Function,
                  length(Args) + 1)
            of
                true ->
                    Reply = erlang:apply(repoxy_core, Function, [State|Args]);
                false ->
                    Reply = {error, {invalid_command, Request}}
            end;
        Other ->
            Reply = {error, {syntax_error, Other}}
    end,
    {reply, Reply, project_loaded, State}.

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
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, project_loaded, _State) ->
    repoxy_core:unload_rebar(),
    ok;
terminate(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
