%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Parse/Generate strings containing s-expressions to/from erlang
%%% terms.
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_sexp_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

test_sexp(0) ->
    union([integer(), atom(), {}]);
test_sexp(Depth) ->
    union(
      lists:foldr(fun(D, Acc) ->
                          Acc ++
                              [test_sexp(D),
                               {test_sexp(D)},
                               {test_sexp(D), test_sexp(D)},
                               {test_sexp(D), test_sexp(D), test_sexp(D)},
                               list(test_sexp(D)),
                               [test_sexp(D) || _ <- lists:seq(1, D)] ++ [test_sexp(D) | test_sexp(D)]]
                  end,
                  [],
                  lists:seq(0, Depth - 1))).

prop_bijective_to_from_str() ->
    numtests(1000,
             ?FORALL(SExp, test_sexp(3),
                     begin
                         {ok, SExp} =:= repoxy_sexp:to_erl(
                                          repoxy_sexp:from_erl(SExp))
                     end)).

valid_msg_test_() ->
    {timeout, 10,
     fun() ->
             proper:quickcheck(prop_bijective_to_from_str(), [verbose, long_result])
     end}.


integer_test() ->
    ?assertEqual("123", repoxy_sexp:from_erl(123)).

integer_0_test() ->
    ?assertEqual("0", repoxy_sexp:from_erl(0)).

empty_list_is_empty_string_test() ->
    ?assertEqual("\"\"", repoxy_sexp:from_erl([])).
