#!/usr/bin/env escript

%% For debugging from Erlang shell.
%-module(erlang_check_file).
%-compile([export_all]).

-export([main/1]).

main([FileName]) ->
    LibDirs = filelib:wildcard("{lib,deps}/*/ebin"),
    compile(FileName, LibDirs);

main([FileName, "-rebar", Path, LibDirs]) ->
    NewLibDirs = LibDirs ++ rebar_lib_dirs(Path),
    io:format("~p~n", [NewLibDirs]),
    compile(FileName, NewLibDirs);

main([FileName, LibDirs]) ->
    compile(FileName, LibDirs).

compile(FileName, LibDirs) ->
    Root = get_root(filename:dirname(FileName)),
    ok = code:add_pathsa(LibDirs),
    compile:file(FileName, [warn_obsolete_guard,
                            warn_unused_import,
                            warn_shadow_vars,
                            warn_export_vars,
                            strong_validation,
                            report,
                            {i, filename:join(Root, "include")},
                            {i, filename:join(Root, "deps")},
                            {i, filename:join(Root, "apps")},
                            {i, filename:join(Root, "lib")}]).

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
    get_dep_dirs(Root, Config) ++
    get_sub_dirs(Root, Config).

get_dep_dirs(Root, Config) ->
    case lists:keyfind(deps, 1, Config) of
        false -> [];
        {deps, Deps} ->
    end.


get_sub_dirs(Root, Config) ->
    case lists:keyfind(sub_dirs, 1, Config) of
        false -> [];
        {sub_dirs, SubDirs} ->
            D = [[filename:join([Root, SubDir]),
                  filename:join([Root, SubDir, "include"]),
                  filename:join([Root, SubDir, "deps"]),
                  filename:join([Root, SubDir, "lib"])]
                 || SubDir <- SubDirs],
            lists:append(D)
    end.
