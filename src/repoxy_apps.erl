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
load_app(ABC = #app_build_cfg{name = Name}) ->
    case application:load(Name) of

        ok ->
            find_assets(ABC);

        Error ->
            %% TODO remove lib paths on error?
            error_logger:error_msg("Could not load application ~p: ~p",
                                   [ABC, Error]),
            repoxy_evt:notify(?on_app_load_failed(Name, Error))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
find_assets(#app_build_cfg{name = Name, lib_paths = LibPaths}) ->
    code:add_pathsz(LibPaths),
    {ok, Keys} = application:get_all_key(Name),
    repoxy_evt:notify(?on_app_load(Name, Keys)).

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
