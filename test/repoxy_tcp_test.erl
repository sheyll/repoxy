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
    Test = self(),

    M = em:new(),

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
    em:await_expectations(M),
    ok = gen_tcp:close(Sock).


close_command_test() ->
    InMsg = "(close)",
    InTerm = [close],
    Test = self(),

    M = em:new(),

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
    em:verify(M).


multi_packet_message_test() ->
    InMsgPart1 = "request p1 ",
    InMsgPart2 = "request p2",
    InTermIncomplete = {error, reason},
    InTerm = request,
    InTermComplete = {ok, InTerm},
    Test = self(),

    M = em:new(),

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
    ok = gen_tcp:send(Sock, InMsgPart1),
    receive after 500 -> ok end,
    ok = gen_tcp:send(Sock, InMsgPart2),
    em:await_expectations(M),
    ok = gen_tcp:close(Sock).
