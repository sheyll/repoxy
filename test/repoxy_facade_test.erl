%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade_test).
-include_lib("eunit/include/eunit.hrl").

invalid_rebar_config_test() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, _}, repoxy_facade:start_link("bad_file")).

change_dir_test() ->
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_rebar, load_rebar, []),
    em:replay(M),
    c:cd("src"),
    ?assertMatch({ok, _}, repoxy_facade:start_link("../rebar.config")),
    ?assert(filelib:is_regular("rebar.config")),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).

rebar_test() ->
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_rebar, load_rebar, []),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link("rebar.config")),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).
