%%%-----------------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Process repoxy commands from inside rebar via hooking into the rebar command
%%% processing pipeline.
%%% @end
%%% Created : 17 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-----------------------------------------------------------------------------

-module(repoxy_rebar_plugin).

-export([repoxy_discover/2]).

-include("repoxy.hrl").
-include("repoxy_project_server.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Report all applications that rebar discovers to repoxy_facade.
%% @end
%%------------------------------------------------------------------------------
repoxy_discover(Cfg, Arg) when is_list(Arg) ->
    IsApp = string:str(Arg, ".app") =/= 0,
    if IsApp ->
            {NewCfg, AppName} = rebar_app_utils:app_name(Cfg, Arg),
            io:format("REPOXY REBAR PLUGIN::repoxy_discover APP: ~p~n",
                      [AppName]),
            AppLibDirs = rebar_config:get_local(NewCfg, lib_dirs, []),
            CWD = filename:absname(rebar_utils:get_cwd()),
            ErlOpts = rebar_utils:erl_opts(NewCfg),
            ErlEUnitOpts = rebar_config:get_local(NewCfg, erl_eunit_opts, []),
            EDocOpts = rebar_config:get_local(NewCfg, edoc_opts, []),
            LibPaths = [rebar_utils:ebin_dir()|
                        expand_lib_dirs(AppLibDirs, CWD, [])],
            SourcePath = filename:join([CWD, "src"]),
            TestPath = filename:join([CWD, "test"]),
            repoxy_project_events:notify(?on_app_discovered(
                                            #app_build_cfg{
                                               name = AppName,
                                               lib_paths = LibPaths,
                                               cwd = CWD,
                                               src_dir = SourcePath,
                                               test_dir = TestPath,
                                               erl_opts = ErlOpts,
                                               erl_eunit_opts = ErlEUnitOpts,
                                               edoc_opts = EDocOpts}));

       true ->
            ok
    end;
repoxy_discover(_Cfg, _Arg) ->
    ok.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    FqApps = [filename:absname(filename:join([Root, A])) || A <- Apps],
    expand_lib_dirs(Rest, Root, Acc ++ FqApps).
