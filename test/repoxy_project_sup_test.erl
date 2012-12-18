%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_project_sup_test).

-include_lib("eunit/include/eunit.hrl").

default_port_test() ->
    process_flag(trap_exit, true),
    M = em:new(),

    em:strict(M, repoxy_project_events, start_link, [],
              {return, {ok, self()}}),
    em:strict(M, repoxy_project, start_link, [],
              {return, {ok, self()}}),
    em:strict(M, repoxy_tcp, start_link, [7979],
              {return, {ok, self()}}),

    em:replay(M),
    Result = repoxy_project_sup:start_link(),
    em:verify(M),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    exit(Pid, kill).

custom_port_test() ->
    process_flag(trap_exit, true),
    M = em:new(),

    em:strict(M, repoxy_project_events, start_link, [],
              {return, {ok, self()}}),
    em:strict(M, repoxy_project, start_link, [],
              {return, {ok, self()}}),
    em:strict(M, repoxy_tcp, start_link, [1234],
              {return, {ok, self()}}),

    em:replay(M),
    ok = application:load(repoxy),
    ok = application:set_env(repoxy, tcp_port, 1234),
    Result = repoxy_project_sup:start_link(),
    em:verify(M),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    exit(Pid, kill).
