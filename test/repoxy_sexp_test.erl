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

-define(SEXP_RND(Depth), sexp(random:uniform(Depth) - 1)).

integer_test() ->
    ?assertEqual("123", repoxy_sexp:from_erl(123)).

integer_0_test() ->
    ?assertEqual("0", repoxy_sexp:from_erl(0)).

empty_list_is_empty_string_test() ->
    ?assertEqual("\"\"", repoxy_sexp:from_erl([])).


strange_0_test() ->
    ?assertMatch({ok, _},
                 repoxy_sexp:to_erl("(([] . '\\035') \"\" . [n])")).

escaped_atom_test() ->
    ?assertEqual({ok, '\202'},
                 repoxy_sexp:to_erl(repoxy_sexp:from_erl('\202'))).

invalid_char_test() ->
    ?assertEqual({error, {illegal,[224]}},
                 repoxy_sexp:to_erl([224])).

bad_syntax_test() ->
    ?assertEqual({error, "syntax error before: ']'"},
                 repoxy_sexp:to_erl("(hello]")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Property based brute force expression crunching...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_and_from_bijection_test_() ->
    {timeout, 10,
     fun() ->
             try
                 proper:quickcheck(prop_bijective_to_from_str(), [verbose, long_result])
             catch
                 C:E ->
                     error_logger:error_report(erlang:get_stacktrace()),
                     erlang:C(E)
             end
     end}.

prop_bijective_to_from_str() ->
    numtests(10000,
             ?FORALL(SExp, sexp(3),
                     begin
                         SExpStr = repoxy_sexp:from_erl(SExp),
                         {ok, SExp} =:= (catch repoxy_sexp:to_erl(SExpStr))
                     end)).
sexp(0) ->
    union([integer(),
           safe_atom(),
           string()
          ]);

sexp(Depth) ->
    union([?SEXP_RND(Depth),
           ?LAZY(list(?SEXP_RND(Depth))),
           ?LAZY(loose_tuple(?SEXP_RND(Depth))),

           %% improper lists
           fixed_list([?SEXP_RND(Depth) || _ <- lists:seq(1, random:uniform(Depth) - 1)]
                       ++ [?SEXP_RND(Depth) | ?SEXP_RND(1)])
          ]).

safe_atom() ->
    ?SUCHTHAT(A, atom(),
              lists:all(fun printable/1, atom_to_list(A))).

printable(Char) when Char > 32 andalso Char < 127 ->
    true;
printable(_) ->
    false.
