%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_tcp_test).

-include_lib("eunit/include/eunit.hrl").

valid_message_test() ->
    InMsg = "request",
    InTerm = request,
    OutTerm = {some_event, earg1},
    OutMsg = "[some_event earg1]",
    Test = self(),

    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_tcp, em:zelf()]),
    %% send an event from repoxy_project_events to tcp client
    em:strict(M, repoxy_sexp, from_erl, [OutTerm],
              {return, OutMsg}),
    %% receive a command from tcp client and dispatch to repoxy_facade
    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {ok, InTerm}}),
    em:strict(M, repoxy_facade, handle_request, [InTerm]),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    repoxy_tcp:on_project_event(whereis(repoxy_tcp), OutTerm),
    ?assertEqual({ok, OutMsg}, gen_tcp:recv(Sock,0)),
    ok = gen_tcp:send(Sock, InMsg),
    em:await_expectations(M),
    ok = gen_tcp:close(Sock),
    kill_repoxy_tcp().

need_more_data_close_reconnect_test() ->
    InMsg = "request",
    InTerm = request,
    Test = self(),

    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_tcp, em:zelf()]),
    %% receive a command from tcp client and dispatch to repoxy_facade
    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {error, test_error}}),
    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {ok, InTerm}}),
    em:strict(M, repoxy_facade, handle_request, [InTerm]),
    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:send(Sock, InMsg),
    receive after 150 -> ok end,
    ok = gen_tcp:close(Sock),
    {ok, Sock2} = gen_tcp:connect("localhost", 5678,
                                  [{active, false},
                                   {mode, list},
                                   {packet, raw}]),
    ok = gen_tcp:send(Sock2, InMsg),
    em:await_expectations(M),
    kill_repoxy_tcp().


close_command_test() ->
    InMsg = "(close)",
    InTerm = [close],
    Test = self(),

    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler,
              [repoxy_tcp, em:zelf()]),
    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {function, fun(_) ->
                                 Test ! closing,
                                 {ok, InTerm}
                         end}),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:send(Sock, InMsg),
    receive closing -> ok end,
    {ok, Sock2} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:close(Sock2),
    em:verify(M),
    kill_repoxy_tcp().


multi_packet_message_test() ->
    InMsgPartRaw1 = "                request p1",
    InMsgPart1 = "request p1",
    InMsgPart2 = "request p2",
    InTermIncomplete = {error, reason},
    InTerm = request,
    InTermComplete = {ok, InTerm},
    Test = self(),

    M = em:new(),
    em:strict(M, repoxy_project_events, add_sup_handler, [repoxy_tcp, em:any()]),
    em:strict(M, repoxy_sexp, to_erl, [InMsgPart1],
              {return, InTermIncomplete}),
    em:strict(M, repoxy_sexp, to_erl, [InMsgPart1 ++ InMsgPart2],
              {return, InTermComplete}),
    em:strict(M, repoxy_facade, handle_request, [InTerm]),
    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {sndbuf, length(InMsgPart1) + 1},
                                  {nodelay, true},
                                  {packet, raw}]),
    ok = gen_tcp:send(Sock, InMsgPartRaw1),
    receive after 500 -> ok end,
    ok = gen_tcp:send(Sock, InMsgPart2),
    em:await_expectations(M),
    ok = gen_tcp:close(Sock),
    kill_repoxy_tcp().

kill_repoxy_tcp() ->
    case whereis(repoxy_tcp) of
        undefined ->
            ok;
        Pid ->
            process_flag(trap_exit, true),
            R = erlang:monitor(process, Pid),
            exit(Pid, stop),
            receive
                {'DOWN', R, _, _, _} ->
                    ok
            end
    end.
