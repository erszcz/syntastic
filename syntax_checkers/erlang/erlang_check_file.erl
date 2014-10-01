#!/usr/bin/env escript

-export([main/1]).

%% Uncomment to turn on debugging.
%-define(DEBUG, true).

-ifdef(DEBUG).
-define(LOG(Fmt, Args), file:write_file("/tmp/erlang_check_file.log",
                                        io_lib:format(Fmt, Args), [append])).
-else.
-define(LOG(Fmt, Args), ok).
-endif. %% DEBUG
-define(LOG(Msg), ?LOG(Msg, [])).

main([FileName]) ->
    ?LOG("main([FileName])~n"),
    ?LOG("cwd: ~p~n", [file:get_cwd()]),
    LibDirs = (["ebin", "include", "src", "test"] ++
               filelib:wildcard("{apps,deps,lib}/*/{ebin,include}")),
    ?LOG("lib dirs: ~p~n", [LibDirs]),
    compile(FileName, LibDirs);

main([FileName, "-rebar", Path, LibDirs]) ->
    ?LOG("main([-rebar])~n"),
    {ok, L} = file:consult(Path),
    P = dict:from_list(L),
    Root = filename:dirname(Path),

    Lib1 = case dict:find(lib_dirs, P) of
             {ok, X} -> lists:map(fun(Sub) -> Root ++ "/" ++ Sub end, X);
             _ -> []
           end,

    Lib2 = case dict:find(sub_dirs, P) of
             {ok, Y} -> lists:foldl(
                          fun(Sub,Sofar) ->
                              Sofar ++ [
                                        Root ++ "/" ++ Sub,
                                        Root ++ "/" ++ Sub ++ "/include",
                                        Root ++ "/" ++ Sub ++ "/deps",
                                        Root ++ "/" ++ Sub ++ "/lib"
                                       ] end, [], Y);
             _ -> []
           end,

    LibDirs1 = LibDirs ++ Lib1 ++ Lib2,
    %io:format("~p~n", [LibDirs1]),
    compile(FileName, LibDirs1);

main([FileName, LibDirs]) ->
    ?LOG("main([FileName, LibDirs])~n"),
    compile(FileName, LibDirs).

compile(FileName, LibDirs) ->
    Root = get_root(),
    ok = code:add_pathsa(LibDirs),
    ?LOG("compile : file ( ~p )", [ [FileName,
                                     [warn_obsolete_guard,
                                      warn_unused_import,
                                      warn_shadow_vars,
                                      warn_export_vars,
                                      strong_validation,
                                      warn_missing_spec,
                                      report] ++
                                     [{i, filename:join(Root, I)} || I <- LibDirs]] ]),
    compile:file(FileName,
                 [warn_obsolete_guard,
                  warn_unused_import,
                  warn_shadow_vars,
                  warn_export_vars,
                  strong_validation,
                  warn_missing_spec,
                  report] ++
                 [{i, filename:join(Root, I)} || I <- LibDirs]).

get_root() ->
    {ok, Dir} = file:get_cwd(),
    Dir.
