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
         start_link/1]).

%% gen_fsm callbacks
-export([init/1,
         project_loaded/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {rebar_config :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

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
    {ok, Req}.

%%--------------------------------------------------------------------
%% @doc
%% Start the facade fsm that handles request for a single rebar
%% project. Currently only one concurrent project and client can
%% exist.
%% @parameter a string containing the absolute path to a rebar config.
%% @end
%%--------------------------------------------------------------------
start_link(RebarFile) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, RebarFile, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init(RebarFile) ->
    stop_on_error(
      load_project(
        check_config(RebarFile))).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
project_loaded(Request, _From, State) ->
    Reply = ok,
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
stop_on_error({error, Reason}) ->
    {stop, {error, Reason}};
stop_on_error(ok) ->
    {ok, project_loaded, #state{}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
load_project(ok) ->
    repoxy_rebar:load_rebar();
load_project(Error) ->
    Error.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
check_config(RebarFile) ->
    case filelib:is_regular(RebarFile) of
        false ->
            {error, "File does not exist"};
        true ->
            AbsName = filename:absname(RebarFile),
            DirName = filename:dirname(AbsName),
            c:cd(DirName),
            ok
    end.
