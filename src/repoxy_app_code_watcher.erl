%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2013, Sven Heyll
%%% @doc
%%% This server watches application load/unload events and detects the erlang sources
%%% attached to the application.
%%% It will watch the source files for changes and it will watch the directories for
%%% addition/removal of sources.
%%% When a source is added or removed, a clean build is triggered. When a
%%% an application descriptor is changed, also a clean build is triggered.
%%% For each application there will be a server that watches the files
%%% @end
%%% Created :  1 Jan 2013 by Sven Heyll <sven@sheyllpc>

-module(repoxy_app_code_watcher).
