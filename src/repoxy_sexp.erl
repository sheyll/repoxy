%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Parse/Generate strings containing s-expressions to/from erlang
%%% terms.
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_sexp).

%% API
-export([to_erl/1,
         from_erl/1]).

-export_type([sexp_term/0,
              sexp/0]).

-type sexp_term() :: integer() | string() | atom() |
                     tuple(sexp_term()) |
                     [sexp_term() | sexp_term()] | [].
-type sexp() :: string().


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert a string containing an s-expression as defined by the
%% *.yrl/*.xrl files in the source folder.
%% @end
%%--------------------------------------------------------------------
-spec to_erl(sexp()) ->
                    {ok, sexp_term()} | {error, term()}.
to_erl(InStr) ->
    {ok, Tokens, _EndLine} = repoxy_sexp_scanner:string(InStr),
    repoxy_sexp_parser:parse(Tokens).

%%--------------------------------------------------------------------
%% @doc
%% Convert an erlang term to an s-expression string().
%% @end
%%--------------------------------------------------------------------
-spec from_erl(sexp_term()) ->
                    sexp().
from_erl(Term) when
      is_atom(Term)
      orelse is_integer(Term) ->
    lists:flatten(io_lib:format("~w", [Term]));

from_erl(Term) when
      is_tuple(Term) ->
    "["
        ++ string:join(lists:map(fun from_erl/1,
                                tuple_to_list(Term)), " ")
        ++ "]";

from_erl(Term) when
      is_list(Term) ->
    case handle_improper(Term) of
        {ok, Heads, Tail} ->
            "("
                ++ string:join(lists:map(fun from_erl/1, Heads), " ")
                ++ " . "
                ++ from_erl(Tail)
                ++ ")";

        is_proper ->
            case is_string(Term) of
                true ->
                    "\"" ++ Term ++ "\"";
                false ->
                    "("
                        ++ string:join(lists:map(fun from_erl/1, Term), " ")
                        ++ ")"
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec is_string(list()) ->
                       boolean().
is_string(List) ->
    lists:all(fun is_char/1, List).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec is_char(integer()) ->
                       boolean().
is_char(I) when
      is_integer(I)
      andalso I > 0
      andalso I < 255 ->
    true;
is_char(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec handle_improper(list()) ->
                             {ok, list(), sexp_term()} | is_proper.
handle_improper(List) ->
    try length(List) of
        N when is_integer(N) ->
            is_proper
    catch
        _:_ ->
            {Heads, Tail} = separate_improper(List, []),
            {ok, Heads, Tail}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
separate_improper([H|T], Acc) when
      is_list(T) ->
    separate_improper(T, [H|Acc]);
separate_improper([H|T], Acc) ->
    {lists:reverse([H|Acc]), T}.
