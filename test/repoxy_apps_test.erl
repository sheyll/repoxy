%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%%-------------------------------------------------------------------
-module(repoxy_apps_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").

load_unload_app_build_cfg_test() ->
    M = em:new(),
    em:strict(M, repoxy_evt, notify,
              [fun(?on_app_load(nano_trace,
                                _AppSpecKeys)) -> true;
                  (_) -> false
               end]),
    em:strict(M, repoxy_evt, notify,
              [?on_app_unload(nano_trace)]),
    em:replay(M),
    %% this assumes that 'nano_trace' is added as dependency!
    repoxy_apps:load_app(
      #app_build_cfg{name = nano_trace,
                     lib_paths = ["../deps/nano_trace/ebin/"]}),
    repoxy_apps:unload_app(
      #app_build_cfg{name = nano_trace,
                     lib_paths = ["../deps/nano_trace/ebin/"]}),
    em:await_expectations(M).

load_twice_test() ->
    M = em:new(),
    em:strict(M, repoxy_evt, notify,
              [fun(?on_app_load(nano_trace,
                                _AppSpecKeys)) -> true;
                  (_) -> false
               end]),
    em:strict(M, repoxy_evt, notify,
              [?on_app_load_failed(
                  nano_trace,
                  {error, {already_loaded, nano_trace}})]),
    em:replay(M),
    %% this assumes that 'nano_trace' is added as dependency!
    repoxy_apps:load_app(
      #app_build_cfg{name = nano_trace,
                     lib_paths = ["../deps/nano_trace/ebin/"]}),
    repoxy_apps:load_app(
      #app_build_cfg{name = nano_trace,
                     lib_paths = ["../deps/nano_trace/ebin/"]}),
    em:await_expectations(M).

load_error_test() ->
    M = em:new(),
    em:strict(M, repoxy_evt, notify,
              [fun(?on_app_load_failed(xxxnano_tracexxx, _)) -> true;
                  (_) -> false
               end]),
    em:replay(M),
    %% this assumes that 'nano_trace' is added as dependency!
    repoxy_apps:load_app(
      #app_build_cfg{name = xxxnano_tracexxx,
                     lib_paths = []}),
    em:await_expectations(M).

unload_error_test() ->
    M = em:new(),
    em:strict(M, repoxy_evt, notify,
              [fun(?on_app_unload_failed(xxxnano_tracexxx, _)) -> true;
                  (_) -> false
               end]),
    em:replay(M),
    %% this assumes that 'nano_trace' is added as dependency!
    repoxy_apps:unload_app(
      #app_build_cfg{name = xxxnano_tracexxx,
                     lib_paths = []}),
    em:await_expectations(M).
