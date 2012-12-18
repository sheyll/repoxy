-module(repoxy_project_code).

-compile([export_all]).
-include("repoxy.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Create a temporary directory for building and compiling and add it to the
%% configuration.
%% @end
%%------------------------------------------------------------------------------
-spec new_build_dir() -> string().
new_build_dir() ->
    {A,B,C} = now(),
    BuildDir = lists:flatten(io_lib:format("repoxy_build_~w_~w_~w", [A,B,C])),
    %% TODO make platform independent
    AbsBuildDir = filename:join(["/tmp", BuildDir]),
    filelib:ensure_dir(filename:join([BuildDir, "stupid-ensure-dir-hack"])),
    code:add_patha(AbsBuildDir),
    AbsBuildDir.

%%------------------------------------------------------------------------------
%% @doc
%% Delete the build directory and remove it from the code path.
%% @end
%%------------------------------------------------------------------------------
-spec clean_build_dir(string()) -> ok.
clean_build_dir(BuildDir)
  when BuildDir =/= undefined ->
    file:del_dir(BuildDir),
    code:del_path(BuildDir),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Save code_path and loaded applications to be restored with `restore_node'.
%% @end
%%------------------------------------------------------------------------------
-spec backup_node() -> #node_backup{}.
backup_node() ->
    #node_backup{apps_loaded = application:loaded_applications(),
                 code_path = code:get_path(),
                 loaded_modules = code:all_loaded()}.

%%------------------------------------------------------------------------------
%% @doc
%% Unload the rebar application from the current node. Also reset the code_path
%% and stop/unload all applications that were not loaded before.
%% @end
%%------------------------------------------------------------------------------
-spec restore_node(#node_backup{}) -> ok.
restore_node(#node_backup{apps_loaded = OldAppsLoaded,
                          code_path = OldCodePath,
                          loaded_modules = OldModules}) ->
    [try
         AppName = element(1, A),
         application:stop(AppName),
         application:unload(AppName)
     catch
         C:E ->
             error_logger:info_msg("Unloading ~p failed: ~p:~p ~n", [A, C, E])
     end
     || A <- application:loaded_applications(),
        not lists:member(A, OldAppsLoaded)],
    code:set_path(OldCodePath),

    [begin
         case string:str(atom_to_list(Mod), "repoxy") of
             0 ->
                 error_logger:info_msg("^Killing ~w ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~n", [Mod]),
                 code:delete(Mod),
                 code:purge(Mod);
             _ ->
                 ignore
         end
     end
     || M = {Mod, _} <- code:all_loaded(),
        not lists:member(M, OldModules)],
    error_logger:info_msg("~n  *** NODE RESETTED ***~n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Load an application into the current node.
%% @end
%%------------------------------------------------------------------------------
load_app(#app_info{name = Name, lib_paths = LibPaths}) ->
    code:add_pathsa(LibPaths),
    dispatch_load_event(Name, application:load(Name)). %% TODO remove lib paths on error?

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dispatch_load_event(AI, Arg = {error, _}) ->
    repoxy_project_events:notify(?on_app_load_failed(AI, Arg)), Arg;
dispatch_load_event(AI, Arg) ->
    repoxy_project_events:notify(?on_app_load(AI)), Arg.

%%------------------------------------------------------------------------------
%% @doc
%% Unload an application from the current node.
%% @end
%%------------------------------------------------------------------------------
unload_app(#app_info{name = Name, lib_paths = LibPaths}) ->
    [code:del_path(LibPath) || LibPath <- LibPaths],
    dispatch_load_event(Name, application:unload(Name)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
dispatch_unload_event(AppName, Arg = {error, _}) ->
    repoxy_project_events:notify(?on_app_unload_failed(AppName, Arg)), Arg;
dispatch_unload_event(AppName, Arg) ->
    repoxy_project_events:notify(?on_app_unload(AppName)), Arg.

%% TODO create app_runtime_info with loaded modules
%% TODO generat on_app_unload when project gets unloaded

%% Client moves point/cursor and sends
%% (lookup-scope app-name unsaved-data-file-name point)
%% client clears current-scope-cache

%% Server detects scope and sends
%% (scope-info + title category line-start line-end)

%% Client sends (lookup_completions scope)

%% Server sends (on_completion_found scope source_template display_title rank)

%% -OR- client knows parse treee and asks semantically
%% !!! NO because: if this logic is in server: much easier to add new client
