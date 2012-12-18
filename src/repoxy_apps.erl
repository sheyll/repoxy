%%------------------------------------------------------------------------------
%% @doc
%% Functions that deal with discovery, loading and unloading of Erlang/OTP
%% applications.
%% @end
%%------------------------------------------------------------------------------
-module(repoxy_apps).

-export([load_app/1, unload_app/1]).

-include("repoxy.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Load an application into the current node.
%% @end
%%------------------------------------------------------------------------------
-spec load_app(#app_build_cfg{}) ->
                      ok.
load_app(#app_build_cfg{name = Name, lib_paths = LibPaths}) ->
    add_lib_paths(
      LibPaths,
      dispatch_load_event(
        Name,
        get_application_descriptor(
          Name,
          application:load(Name)))),
    ok. %% TODO remove lib paths on error?

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_application_descriptor(Name, ok) ->
    application:get_all_key(Name);
get_application_descriptor(_Name, Err = {error, _}) ->
    Err.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dispatch_load_event(Name, Arg = {error, _}) ->
    repoxy_evt:notify(?on_app_load_failed(Name, Arg)), Arg;
dispatch_load_event(Name, {ok, AppKeys} = Arg) ->
    repoxy_evt:notify(?on_app_load(Name, AppKeys)), Arg.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_lib_paths(_, Err = {error, _}) ->
    Err;
add_lib_paths(LibPaths, {ok, _}) ->
    code:add_pathsz(LibPaths).

%%------------------------------------------------------------------------------
%% @doc
%% Unload an application from the current node.
%% @end
%%------------------------------------------------------------------------------
-spec unload_app(#app_build_cfg{}) ->
                        ok.
unload_app(#app_build_cfg{name = Name, lib_paths = LibPaths}) ->
    [code:del_path(LibPath) || LibPath <- LibPaths],
    dispatch_unload_event(Name, application:unload(Name)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dispatch_unload_event(AppName, Arg = {error, _}) ->
    repoxy_evt:notify(?on_app_unload_failed(AppName, Arg)), Arg;
dispatch_unload_event(AppName, Arg) ->
    repoxy_evt:notify(?on_app_unload(AppName)), Arg.

%% TODO create app_runtime_info with loaded modules
%% TODO generat on_app_unload when project gets unloaded

%% Client moves point/cursor and sends
%% (lookup-scope app-name unsaved-data-file-name point)
%% client clears current-scope-cache

%% Server detects scope and sends
%% (scope-info + title category line-start line-end)

%% Client sends (lookup_completions scope)

%% Server sends (on_completion_found scope source_template display_title rank)

%% -OR- client knows parse treee and asks semantically
%% !!! NO because: if this logic is in server: much easier to add new client
