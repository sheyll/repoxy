%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% This facade simply wraps calls to rebar. Rebar cannot be mocked
%%% because the unit tests are actually run by rebar, therefore all
%%% rebar invokations are contained in this module.
%%% @end
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_rebar).

-export([load_rebar/0]).


load_rebar() ->
    rebar:main(["compile"]),
    error_logger:info_msg("Started rebar~n").
