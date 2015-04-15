%#!/usr/bin/env escript

%% For debugging from Erlang shell.
-module(erlang_check_file).
-compile([export_all]).

-export([main/1]).

main([FileName]) ->
    LibDirs = filelib:wildcard("{lib,deps}/*/{ebin,include}"),
    compile(FileName, LibDirs);

main([FileName, "-rebar", Path, _LibDirs]) ->
    {EbinDirs, IncludeDirs} = rebar_lib_dirs(Path),
    compile(FileName, EbinDirs, IncludeDirs);

main([FileName, LibDirs]) ->
    compile(FileName, LibDirs).

compile(FileName, EbinDirs, IncludeDirs) ->
    ok = code:add_pathsa(EbinDirs),
    compile:file(FileName,
                 [warn_obsolete_guard,
                  warn_unused_import,
                  warn_shadow_vars,
                  warn_export_vars,
                  strong_validation,
                  report] ++
                 [{i, I} || I <- IncludeDirs]).

compile(FileName, LibDirs) ->
    Root = get_root(filename:dirname(FileName)),
    ok = code:add_pathsa(LibDirs),
    compile:file(FileName,
                 [warn_obsolete_guard,
                  warn_unused_import,
                  warn_shadow_vars,
                  warn_export_vars,
                  strong_validation,
                  report] ++
                 [{i, filename:join(Root, I)} || I <- LibDirs]).

get_root(Dir) ->
    Path = filename:split(filename:absname(Dir)),
    filename:join(get_root(lists:reverse(Path), Path)).

get_root([], Path) ->
    Path;
get_root(["src" | Tail], _Path) ->
    lists:reverse(Tail);
get_root(["test" | Tail], _Path) ->
    lists:reverse(Tail);
get_root([_ | Tail], Path) ->
    get_root(Tail, Path).

rebar_lib_dirs(Path) ->
    Root = filename:dirname(Path),
    FileName = filename:basename(Path),
    %% We can't match on string tails but we can on heads.
    {ok, Config} = case lists:reverse(FileName) of
                       %% *.config.script
                       "tpircs.gifnoc." ++ _ ->
                           file:script(Path);
                       %% *.config
                       "gifnoc." ++ _ ->
                           file:consult(Path)
                   end,
    group_by_ebin_include(get_dep_dirs(Root, Config) ++
                          get_sub_dirs(Root, Config)).

group_by_ebin_include(Dirs) ->
    lists:foldl(fun ebin_or_include/2, {[], []}, Dirs).

ebin_or_include({ebin, Dir}, {Ebin, Include}) ->
    {[Dir | Ebin], Include};
ebin_or_include({include, Dir}, {Ebin, Include}) ->
    {Ebin, [Dir | Include]}.

get_dep_dirs(Root, Config) ->
    case lists:keyfind(deps, 1, Config) of
        false -> [];
        {deps, Deps} ->
            D = [[{ebin, filename:join([Root, dep_name(Dep), "ebin"])},
                  {include, filename:join([Root, dep_name(Dep), "include"])},
                  {include, filename:join([Root, dep_name(Dep), "src"])}]
                 || Dep <- Deps],
            lists:append(D)
    end.

dep_name({DepName, _, _}) when is_atom(DepName) ->
    atom_to_list(DepName);
dep_name({DepName, _, _}) when is_list(DepName) ->
    DepName.

get_sub_dirs(Root, Config) ->
    case lists:keyfind(sub_dirs, 1, Config) of
        false -> [];
        {sub_dirs, SubDirs} ->
            D = [[{ebin, filename:join([Root, SubDir, "ebin"])},
                  {include, filename:join([Root, SubDir, "include"])},
                  {include, filename:join([Root, SubDir, "src"])}]
                 || SubDir <- SubDirs],
            lists:append(D)
    end.
