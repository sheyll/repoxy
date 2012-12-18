%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%%
%%% Front-End module for Repoxy. Once a server is running, these functions can
%%% be called to dispatch commands to the project server.
%%%
%%% This is server is necessary to coordinate parallel actions so they do not
%%% overwrite on the same source files.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(repoxy_project).

-export([start_link/0, clean_build/0, load/1, unload/0]).

-include("repoxy_project_server.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Start a project server.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, repoxy_project_server, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Initiate rebar and create the temporary build directory. This should be
%% called at the beginning of working with a project, its counterpart is {@link
%% unload/1}. This operation will crash the server when a project is already
%% loaded.
%% @end
%%--------------------------------------------------------------------
load(Dir) ->
    gen_fsm:send_event(?SERVER, ?load(Dir)).

%%--------------------------------------------------------------------
%% @doc

%% Initiate a cleanup and rebuild of the project. All applications will be
%% compiled, all dependencies will be fechted and compiled.  After that every
%% module will be compiled with debug options and loaded. During this command
%% application discovery events for each application and each release will be
%% issued via {@link repoxy_evt}. When the application discovery is
%% finished, repoxy will compile all files and send compilation messages like
%% error or warnings via the gen_event. Note that this command can be executed
%% only when after the project is loaded.

%% @end
%%--------------------------------------------------------------------
clean_build() ->
    gen_fsm:send_event(?SERVER, ?clean_build).

%%--------------------------------------------------------------------
%% @doc
%% remove all trails that a project was loaded via {@link load/1}.
%% @end
%%--------------------------------------------------------------------
unload() ->
    gen_fsm:send_event(?SERVER, ?unload).
