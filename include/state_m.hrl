-ifndef(STATE_M_INCLUDED).
-define(STATE_M_INCLUDED, 1).
%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Some macros that mimic state monad functions.
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------

-define(do(Initial, Funs),
        begin
            lists:foldr(fun(Fun, Acc) ->
                                Fun(Acc)
                        end,
                        Initial,
                        Funs)
         end).

-endif.
