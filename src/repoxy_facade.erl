%% @doc Interpret erlang terms as Lisp like function call to funcitons in {@link
%% repoxy_project}
-module(repoxy_facade).

-export([handle_request/1,
         format_event/1]).

-include("repoxy.hrl").

%% @doc Interpret 'Term' as a Lisp-like function call. The first element of the
%% list must be an atom, an exported function in the module {@link
%% repoxy_project}. All other elements are arguments to that function.  All
%% errors are ignored, as well as all return values from the function.
-spec handle_request([term()]) ->
                            ok.
handle_request([Command|Args]) ->
    Arity = length(Args),
    try erlang:apply(fun repoxy_project:Command/Arity, Args) of
        Res ->
            error_logger:info_msg("Invoked repoxy_project:~w/~w on ~p:~n~p~n~n",
                                  [Command, Arity, Args, Res])
    catch
        C:E ->
            error_logger:error_msg("Invoked repoxy_project:~w/~w on ~p:!! CAUGHT EXCEPTION!!~n~p:~p~n~n",
                                   [Command, Arity, Args, C, E])
    end,
    ok;
handle_request(Other) ->
    error_logger:error_msg("Invalid request:~n~p~n~n", [Other]),
    ok.


%% @doc Format events from {@link repoxy_project_events} into the
%% 'outside-world' representation. If the term is not recognized it will be
%% returned as is.
-spec format_event(term()) -> term().
format_event(?on_app_discovered(AppInfo)) ->
    ?on_app_discovered(format_event(AppInfo));
format_event(#app_info{name = Name,
                       version = Version,
                       cwd = Cwd,
                       src_dir = Src,
                       test_dir = Test}) ->
    [app_info,
     name, Name,
     version, Version,
     cwd, Cwd,
     src_dir, Src,
     test_dir, Test];
format_event(Other) ->
    Other.
