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
    OutTerm = the_response,
    OutMsg = "the_response",
    Test = self(),

    M = em:new(),

    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {ok, InTerm}}),
    em:strict(M, repoxy_facade, handle_request, [InTerm],
              {return, OutTerm}),
    em:strict(M, repoxy_sexp, from_erl, [OutTerm],
              {return, OutMsg}),
    em:strict(M, repoxy_facade, handle_request, [[reset]],
              {function, fun(_) ->
                                 Test ! resetted
                         end}),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:send(Sock, InMsg),
    ?assertEqual({ok, OutMsg}, gen_tcp:recv(Sock, 0)),
    ok = gen_tcp:close(Sock),
    receive
        resetted ->
            ok
    end,
    em:verify(M).


close_command_test() ->
    InMsg = "(close)",
    InTerm = [close],
    OutTerm = ignore,
    Test = self(),

    M = em:new(),

    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {ok, InTerm}}),
    em:strict(M, repoxy_facade, handle_request, [[reset]],
              {return, OutTerm}),
    em:strict(M, repoxy_facade, handle_request, [[reset]],
              {function, fun(_) ->
                                 Test ! resetted
                         end}),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:send(Sock, InMsg),

    receive after 250 -> ok end,
    {ok, Sock2} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {mode, list},
                                  {packet, raw}]),
    ok = gen_tcp:close(Sock2),

    receive
        resetted ->
            ok
    end,
    em:verify(M).


multi_packet_message_test() ->
    InMsgPart1 = "request p1 ",
    InMsgPart2 = "request p2",
    InTermIncomplete = {error, reason},
    InTerm = request,
    InTermComplete = {ok, InTerm},
    OutTerm = response,
    OutMsg = "response",
    Test = self(),

    M = em:new(),

    em:strict(M, repoxy_sexp, to_erl, [InMsgPart1],
              {return, InTermIncomplete}),
    em:strict(M, repoxy_sexp, to_erl, [InMsgPart1 ++ InMsgPart2],
              {return, InTermComplete}),
    em:strict(M, repoxy_facade, handle_request, [InTerm],
              {return, OutTerm}),
    em:strict(M, repoxy_sexp, from_erl, [OutTerm],
              {return, OutMsg}),
    em:strict(M, repoxy_facade, handle_request, [[reset]],
              {function, fun(_) ->
                                 Test ! resetted
                         end}),

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
    ?assertEqual({ok, OutMsg}, gen_tcp:recv(Sock, 0)),
    ok = gen_tcp:close(Sock),
    receive
        resetted ->
            ok
    end,
    em:verify(M).
