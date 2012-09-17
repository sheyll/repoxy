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

    M = em:new(),

    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, {ok, InTerm}}),
    em:strict(M, repoxy_facade, handle_request, [InTerm],
              {return, OutTerm}),
    em:strict(M, repoxy_sexp, from_erl, [OutTerm],
              {return, OutMsg}),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {packet, line}]),
    ok = gen_tcp:send(Sock, InMsg),
    ?assertEqual({ok, OutMsg}, gen_tcp:recv(Sock, 0)),
    ok = gen_tcp:close(Sock),
    em:verify(M).


message_syntax_error_test() ->
    InMsg = "request",
    OutTerm = {error, reason},
    OutMsg = "response",

    M = em:new(),

    em:strict(M, repoxy_sexp, to_erl, [InMsg],
              {return, OutTerm}),
    em:strict(M, repoxy_sexp, from_erl, [OutTerm],
              {return, OutMsg}),

    em:replay(M),
    (catch repoxy_tcp:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 5678,
                                 [{active, false},
                                  {packet, line}]),
    ok = gen_tcp:send(Sock, InMsg),
    ?assertEqual({ok, OutMsg}, gen_tcp:recv(Sock, 0)),
    ok = gen_tcp:close(Sock),
    em:verify(M).
