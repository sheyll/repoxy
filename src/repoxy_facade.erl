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
%         push_app_info/1,
         start_link/0]).

%% gen_fsm callbacks
-export([init/1,
 %        project_loaded/2,
         project_loaded/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {project_cfg :: repoxy_core:project_cfg(),
                node_backup :: repoxy_core:node_backup()}).

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
    {ok, project_loaded, init_node()}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
project_loaded([reset], _From, State) ->
    ok = repoxy_core:restore_node(State#state.node_backup),
    {reply, ok, project_loaded, init_node()};
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
                    Reply = erlang:apply(repoxy_core,
                                         Function,
                                         [State#state.project_cfg | Args]);
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

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init_node() ->
    NodeBackUp = repoxy_core:backup_node(),
    Cfg = repoxy_core:load_rebar(),
    #state{node_backup = NodeBackUp,
           project_cfg = Cfg}.
