%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade_test).
-include_lib("eunit/include/eunit.hrl").

load_test() ->
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).

reset_test() ->
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),

    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:strict(M, repoxy_core, restore_node, [node_data],
              {return, ok}),
    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),

    em:replay(M),
    Res = repoxy_facade:start_link(),
    ?assertMatch({ok, _}, Res),
    ?assertMatch(ok, repoxy_facade:handle_request([reset])),
    em:verify(M).

dispatch_commands_test() ->
    Response = {ok, [ooo, aaa]},
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:strict(M, repoxy_core, xxx, [config, yyy, zzz],
             {return, Response}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    ?assertEqual(Response,
                 repoxy_facade:handle_request([xxx,yyy,zzz])),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).

invalid_command_test() ->
    Response = {error, {syntax_error, xxx}},
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    ?assertEqual(Response,
                 repoxy_facade:handle_request(xxx)),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).

missing_function_test() ->
    Response = {error, {invalid_command, [xxx,yyy,zzz]}},
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, backup_node, [],
              {return, node_data}),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    ?assertEqual(Response,
                 repoxy_facade:handle_request([xxx,yyy,zzz])),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).
