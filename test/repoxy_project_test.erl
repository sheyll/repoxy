%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_project_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").
-include("../src/repoxy_project_server.hrl").

%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test() ->
    M = em:new(),
    mock_add_handler(M),
    em:replay(M),
    Pid = start_server(),
    stop_server(Pid),
    em:verify(M).

load_test() ->
    M = em:new(),
    mock_add_handler(M),
    mock_load_project(M, project_dir),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    stop_server(Pid),
    await_unload(),
    em:verify(M).

unload_test() ->
    M = em:new(),
    mock_add_handler(M),
    mock_load_project(M, project_dir),
    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    repoxy_project:unload(project_dir),
    em:verify(M),
    stop_server(Pid).

clean_build_test() ->
    Self = self(),
    M = em:new(),
    mock_add_handler(M),
    mock_load_project(M, project_dir),
    em:strict(M, repoxy_project_rebar, rebar, [rebar_cfg, ['get-deps', 'compile']],
              {function, fun(_) ->
                                 gen_fsm:send_event(?SERVER, ?app_found(app_info_1))
                         end}),
    em:strict(M, repoxy_project_code, add_app_info, [app_info_1, em:any()],
              {function, fun([_, Cfg]) -> Cfg#prj_cfg{app_infos=[app_info_1]} end}),
    em:strict(M, repoxy_project_code, load_app_into_node, [app_info_1]),
    em:strict(M, my_handler, handle_event, [?on_app_info(app_info_1), state],
              {function, fun(_) -> Self ! passed, {ok, state} end}),

    mock_unload_project(M),
    em:replay(M),
    Pid = start_server(),
    repoxy_project:load(project_dir),
    repoxy_project:clean_build(),
    receive passed -> ok end,
    stop_server(Pid),
    em:verify(M).

%% Test utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mock_add_handler(M) ->
    em:strict(M, my_handler, init, [my_arg], {return, {ok, state}}).

mock_load_project(M, Dir) ->
    em:strict(M, repoxy_project_code, new_build_dir, [], {return, build_dir}),
    em:strict(M, repoxy_project_code, backup_node, [], {return, node_backup}),
    em:strict(M, repoxy_project_rebar, load_rebar, [Dir],
              {return, rebar_cfg}),
    em:strict(M, my_handler, handle_event, [?on_load, state],
              {return, {ok, state}}).

mock_unload_project(M) ->
    Self = self(),
    em:strict(M, repoxy_project_code, restore_node, [node_backup]),
    em:strict(M, repoxy_project_code, clean_build_dir, [build_dir]),
    em:strict(M, my_handler, handle_event, [?on_unload, state],
              {function, fun(_) -> Self ! on_unload, {ok, state} end}).

await_unload() ->
    receive
        on_unload -> ok
    end.

start_server() ->
    process_flag(trap_exit, true),
    {ok, Pid} = repoxy_project_sup:start_link(),
    repoxy_project_events:add_sup_handler(my_handler, my_arg),
    Pid.

stop_server(Pid) ->
    exit(Pid, normal),
    receive
        {'EXIT', Pid, normal} -> ok
    end.

%% scan_project_test() ->
%%     M = em:new(),
%%     em:strict(M, my_handler, init, [my_arg], {return, {ok, state}}),

%%     em:strict(M, repoxy_project_code, backup_node, [], {ok, node_backup}),
%%     em:strict(M, repoxy_project_rebar, load_rebar, [em:any()]),

%%     em:replay(M),
%%     repoxy_project_sup:start_link(project_dir),
%%     repoxy_project_events:add_sup_handler(my_handler, my_arg),
%%     em:verify(M).
