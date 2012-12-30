%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").
-include("../src/repoxy_project_server.hrl").

%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_existing_function_test() ->
    M = em:new(),
    em:strict(M, repoxy_project, existing_fun, [arg1, arg2, arg3]),
    em:replay(M),
    repoxy_facade:handle_request([existing_fun, arg1, arg2, arg3]),
    em:verify(M).

handle_existing_function_exception_test() ->
    M = em:new(),
    em:strict(M, repoxy_project, existing_fun, [],
              {function, fun(_) -> throw(xxx) end}),
    em:replay(M),
    repoxy_facade:handle_request([existing_fun]),
    em:verify(M).

handle_non_existing_function_test() ->
    repoxy_facade:handle_request([non_existing_fun, arg1]).

format_event_passthrough_test() ->
    ?assertEqual(anything_else,
                 repoxy_facade:format_event(anything_else)).

format_event_app_build_cfg_test() ->
    AppInfo = #app_build_cfg{name = name,
                             cwd = "cwd",
                             src_dir = "src",
                             test_dir = "test"},
    Res = repoxy_facade:format_event(?on_app_discovered(AppInfo)),
    ?assertEqual(?on_app_discovered(
                    ['app_build_cfg',
                     'name', "name",
                     'cwd', "cwd",
                     'src_dir', "src",
                     'test_dir', "test"]), Res),
    ok.

format_event_prj_load_test() ->
    RebarCfg = rebar_cfg,
    BaseDir = base_dir,
    M = em:new(),
    em:strict(M, repoxy_rebar_cfg, get_base_dir, [RebarCfg],
              {return, BaseDir}),
    em:replay(M),
    PC = #prj_cfg{rebar_cfg = RebarCfg,
                  build_dir = "build/dir"},
    Res = repoxy_facade:format_event(?on_project_load(PC)),
    ?assertEqual(?on_project_load(
                    ['prj_cfg',
                     'base_dir', BaseDir,
                     'build_dir', "build/dir"]), Res),
    em:verify(M).

format_event_prj_unload_test() ->
    RebarCfg = rebar_cfg,
    BaseDir = base_dir,
    M = em:new(),
    em:strict(M, repoxy_rebar_cfg, get_base_dir, [RebarCfg],
              {return, BaseDir}),
    em:replay(M),
    PC = #prj_cfg{rebar_cfg = RebarCfg,
                  build_dir = "build/dir"},
    Res = repoxy_facade:format_event(?on_project_unload(PC)),
    ?assertEqual(?on_project_unload(
                    ['prj_cfg',
                     'base_dir', BaseDir,
                     'build_dir', "build/dir"]), Res),
    em:verify(M).
