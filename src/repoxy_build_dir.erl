%%%------------------------------------------------------------------------------
%%% @doc
%%% Functions to deal with build directories.
%%% @end
%%%------------------------------------------------------------------------------

-module(repoxy_build_dir).

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
