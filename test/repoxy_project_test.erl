%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_project_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/repoxy.hrl").
-include("../src/repoxy_project_server.hrl").

%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_test() ->
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    em:await(M, PrjLoaded),
    stop_server(Pid),
    em:await_expectations(M).

unexpected_event_test() ->
    UM = unexpected_message,
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),
    em:strict(M, repoxy_project_events, notify,
              [?on_internal_error({error, {unexpected_event, UM}})]),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    gen_fsm:send_event(whereis(repoxy_project_server), UM),
    em:await(M, PrjLoaded),
    stop_server(Pid),
    em:await_expectations(M).

unload_test() ->
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    em:await(M, PrjLoaded),
    repoxy_project:unload(),
    stop_server(Pid),
    em:await_expectations(M).

rebar_error_test() ->
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),
    em:strict(M, repoxy_project_rebar, rebar,
              [rebar_cfg, ['clean', 'get-deps', 'compile', 'repoxy_discover']],
              {return, {error, test_err}}),
    GotProjectEvent = em:strict(M, repoxy_project_events, notify,
                                [?on_internal_error({error, test_err})]),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    em:await(M, PrjLoaded),
    repoxy_project:clean_build(),
    em:await(M, GotProjectEvent),
    stop_server(Pid),
    em:await_expectations(M).

clean_build_test() ->
    AppInfo1 = #app_build_cfg{name = "TestApp1"},
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),

    RebarExecuted = em:strict(M, repoxy_project_rebar, rebar,
                              [rebar_cfg, ['clean', 'get-deps',
                                           'compile', 'repoxy_discover']]),
    Loaded = em:strict(M, repoxy_project_code, load_app, [AppInfo1]),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    em:await(M, PrjLoaded),
    repoxy_project:clean_build(),
    em:await(M, RebarExecuted),
    %% discovery of 'app_build_cfg_1'
    repoxy_project_server:on_project_event(Pid, ?on_app_discovered(AppInfo1)),
    em:await(M, Loaded),
    stop_server(Pid),
    em:await_expectations(M).

unload_existing_app_test() ->
    AppInfo1 = #app_build_cfg{name = "TestApp1"},
    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_project_server, em:zelf()]),
    PrjLoaded = mock_load_project(M, project_dir),
    RebarExecuted = em:strict(M, repoxy_project_rebar, rebar,
                              [rebar_cfg, ['clean', 'get-deps',
                                           'compile', 'repoxy_discover']]),
    Loaded_1st = em:strict(M, repoxy_project_code, load_app, [AppInfo1]),
    %% loading the same app 2. time: must unload before loading again
    em:strict(M, repoxy_project_code, unload_app, [AppInfo1]),
    Loaded_2nd = em:strict(M, repoxy_project_code, load_app, [AppInfo1]),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    em:await(M, PrjLoaded),
    repoxy_project:clean_build(),
    em:await(M, RebarExecuted),
    %% discovery of 'app_build_cfg_1'
    repoxy_project_server:on_project_event(Pid, ?on_app_discovered(AppInfo1)),
    em:await(M, Loaded_1st),
    %% fake a second discovery of the same app
    repoxy_project_server:on_project_event(Pid, ?on_app_discovered(AppInfo1)),
    em:await(M, Loaded_2nd),
    stop_server(Pid),
    em:await_expectations(M).

%% Test utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mock_load_project(M, Dir) ->
    em:strict(M, repoxy_project_code, new_build_dir, [], {return, build_dir}),
    em:strict(M, repoxy_project_code, backup_node, [], {return, node_backup}),
    em:strict(M, repoxy_project_rebar, load_rebar, [Dir],
              {return, rebar_cfg}),
    em:strict(M, repoxy_project_events, notify,
              [?on_project_load(#prj_cfg{build_dir = build_dir,
                                         rebar_cfg = rebar_cfg})]).

mock_unload_project(M) ->
    em:strict(M, repoxy_project_code, restore_node, [node_backup]),
    em:strict(M, repoxy_project_code, clean_build_dir, [build_dir]),
    em:strict(M, repoxy_project_events, notify,
              [fun(?on_project_unload(
                      #prj_cfg{build_dir = build_dir,
                               rebar_cfg = rebar_cfg})) ->
                       true;
                  (_) ->
                       false
               end]).

start_server() ->
    process_flag(trap_exit, true),
    {ok, Pid} = repoxy_project:start_link(),
    Pid.

stop_server(Pid) ->
    exit(Pid, normal),
    receive
        {'EXIT', Pid, normal} -> ok
    after 1000 ->
            throw(expected_stop_server)
    end.
