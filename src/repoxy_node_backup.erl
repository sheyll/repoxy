%%------------------------------------------------------------------------------
%% @doc
%% Functions to backup and restore the state of an Erlang node.
%% @end
%%------------------------------------------------------------------------------
-module(repoxy_node_backup).

-compile([export_all]).
-include("repoxy.hrl").

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
