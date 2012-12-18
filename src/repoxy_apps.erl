%%------------------------------------------------------------------------------
%% @doc
%% Functions that deal with discovery, loading and unloading of Erlang/OTP
%% applications.
%% @end
%%------------------------------------------------------------------------------
-module(repoxy_apps).

-compile([export_all]).
-include("repoxy.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Load an application into the current node.
%% @end
%%------------------------------------------------------------------------------
load_app(#app_build_cfg{name = Name, lib_paths = LibPaths}) ->
    code:add_pathsa(LibPaths),
    dispatch_load_event(Name, application:load(Name)). %% TODO remove lib paths on error?

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dispatch_load_event(AI, Arg = {error, _}) ->
    repoxy_evt:notify(?on_app_load_failed(AI, Arg)), Arg;
dispatch_load_event(AI, Arg) ->
    repoxy_evt:notify(?on_app_load(AI)), Arg.

%%------------------------------------------------------------------------------
%% @doc
%% Unload an application from the current node.
%% @end
%%------------------------------------------------------------------------------
unload_app(#app_build_cfg{name = Name, lib_paths = LibPaths}) ->
    [code:del_path(LibPath) || LibPath <- LibPaths],
    dispatch_load_event(Name, application:unload(Name)).

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
