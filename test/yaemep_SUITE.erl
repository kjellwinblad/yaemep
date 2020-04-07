%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB and Kjell Winblad (http://winsh.me,
%% kjellwinblad@gmail.com) 2019. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(yaemep_SUITE).

%% Code in this file has been copied from emacs_SUITE.erl file in the
%% Erlang/OTP repository

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([test_complete_everything/1,
         test_complete_var/1,
         test_complete_local_function/1,
         test_complete_module/1,
         test_complete_functions_in_module/1,
         test_complete_functions_in_module_from_erl_file/1,
         test_compile_and_load/1,
         test_find_files_when_symlinks/1,
         test_find_files_patterns/1
        ]).

all() ->
    [
     test_complete_everything,
     test_complete_var,
     test_complete_local_function,
     test_complete_module,
     test_complete_functions_in_module,
     test_complete_functions_in_module_from_erl_file,
     test_compile_and_load,
     test_find_files_when_symlinks,
     test_find_files_patterns
    ].

init_per_testcase(_Case, Config) ->
    {ok, CurrentDir} = file:get_cwd(),
    YaemepEl = filename:join([CurrentDir, "..", "..","yaemep.el"]),
    os:cmd(io_lib:format("~s --batch --quick --eval \"\"",
                         [filename:join([CurrentDir,
                                         "..",
                                         "..",
                                         "example",
                                         "emacs_with_erlang_and_yaemep.sh"])])),
    case file:read_file_info(YaemepEl) of
        {ok, _} ->
            case emacs_version_ok(24.3) of
                false -> {skip, "Old or no emacs found"};
                _ -> [{el, YaemepEl}|Config]
            end;
        _ ->
            {skip, "Could not find yaemep.el"}
    end.

end_per_testcase(_Case, _Config) ->
    ok.

emacs_version_ok(AcceptVer) ->
    VersionLine = os:cmd("emacs --version | head -1"),
    io:format("~s~n", [VersionLine]),
    case VersionLine of
        "GNU Emacs " ++ Ver ->
            case string:to_float(Ver) of
                {Vsn, _} when Vsn >= AcceptVer ->
                    Vsn;
                _ ->
                    false
            end;
        Res ->
            io:format("Emacs version fail~n~s~n~n",[Res]),
            false
    end.

dquote(Str) ->
    "\"" ++ Str ++ "\"".


erlang_mode_dir() ->
    erlang:hd(filelib:wildcard(filename:join([emacs_dir(),
                                              "example",
                                              "emacs.d",
                                              "elpa",
                                              "erlang*"]))).

emacs(EmacsCmds) when is_list(EmacsCmds) ->
    Cmd = ["emacs ",
           "--batch --quick ",
           "--directory ", dquote(erlang_mode_dir()), " ",
           "--directory ", dquote(emacs_dir()), " ",
           "--eval \"(require 'yaemep)\" "
           | EmacsCmds],
    Res0 = os:cmd(Cmd ++ " ; echo $?"),
    Rows = string:lexemes(Res0, ["\r\n", $\n]),
    Res = lists:last(Rows),
    Output = string:join(lists:droplast(Rows), "\n"),
    io:format("Cmd ~ts:~n  => ~s ~ts~n", [Cmd, Res, Output]),
    "0" = Res,
    Output.

emacs_dir() ->
    {ok, CurrentDir} = file:get_cwd(),
    filename:join([CurrentDir, "..", ".."]).

test_compile_and_load(_Config) ->
    Dir = emacs_dir(),
    Files = filelib:wildcard("*.el", Dir),
    Compile = fun(File) ->
                      emacs([" -f batch-byte-compile ", dquote(filename:join(Dir, File))]),
                      true
              end,
    lists:foreach(Compile, Files),
    emacs(["-l ", dquote(filename:join(Dir, "yaemep.elc")),
           " -l ", dquote(filename:join(Dir, "yaemep-completion-mode.elc")),
           " -l ", dquote(filename:join(Dir, "yaemep-etags-auto-gen-mode.elc")),
           " -l ", dquote(filename:join(Dir, "yaemep-extra-erlang-menu-mode.elc"))]),
    ElcFiles = filelib:wildcard("*.elc", Dir),
    Delete =
        fun(File) ->
                file:delete(filename:join(Dir, File))
        end,
    lists:foreach(Delete, ElcFiles),
    ok.

test_find_files_when_symlinks(Config) ->
    Files1 = ["d/2 -> ..",
              "d/x.dummy"],
    DirWithSymLoops1 = setup_files(Files1, Config),
    %% Expect (only) one match despite a symlink loop (even for a
    %% non-symlink-aware implementation, directory traversal will
    %% eventually terminate with an eloop error)
    ["d/x.dummy"] =
        run_emacs_support_escript_find_files(
          DirWithSymLoops1,
          filename:join([DirWithSymLoops1, "**", "*.dummy"])),

    %% Now check that non-loop symlinks within the directory structure
    %% are found, and found only once.
    Files2 = ["d/an-alias -> ./dd2",
              "d/dd2/f1.dummy"],
    DirWithSymLoops2 = setup_files(Files2, Config),
    ["d/dd2/f1.dummy"] =  % should not be found as d/an-alias/f1.dummy
        run_emacs_support_escript_find_files(
          DirWithSymLoops2,
          filename:join([DirWithSymLoops2, "**", "*.dummy"])),

    %% Now check that symlinks outside the top dir are followed
    Files3 = ["inside/l -> ../outside",
              "inside/some_file.dummy",
              "outside/other_file.dummy"],
    DirWithSymLoops3 = setup_files(Files3, Config),
    ["inside/some_file.dummy",
     "outside/other_file.dummy"] =
        run_emacs_support_escript_find_files(
          DirWithSymLoops3,
          filename:join([DirWithSymLoops3, "inside", "**", "*.dummy"])),

    %% Canonicalization of ../ and ./ in symlinks
    Files4 = ["inside/l -> ../inside/./../inside/../outside",
              "outside/f.dummy"],
    DirWithSymLoops4 = setup_files(Files4, Config),
    ["outside/f.dummy"] =
        run_emacs_support_escript_find_files(
          DirWithSymLoops4,
          filename:join([DirWithSymLoops4, "inside", "**", "*.dummy"])),
    ok.

test_find_files_patterns(Config) ->
    Files = ["d/a/b/include/x.hrl",
             "d/a/b/src/x.erl",
             "d/a/b/ebin/x.beam",
             "g/c/d/include/y.hrl",
             "g/c/f/include/z.hrl",
             "doc/a.html",
             "README.md"],
    DirTop = setup_files(Files, Config),

    %% Alternative suffices in the patterns (any levels down)
    ["d/a/b/include/x.hrl",
     "d/a/b/src/x.erl",
     "g/c/d/include/y.hrl",
     "g/c/f/include/z.hrl"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "**", "*.{erl,hrl}"])),

    %% Just a file suffix (any levels down)
    ["d/a/b/ebin/x.beam"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "**", "*.beam"])),

    %% Just a file suffix (just one level down)
    ["doc/a.html"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "*", "*.html"])),

    %% Base name is a not a pattern (any levels down)
    ["d/a/b/ebin/x.beam"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "**", "x.beam"])),

    %% No pattern (in top dir, no levels down)
    ["README.md"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "README.md"])),

    %% Handles also complicated patterns (falling back to filelib:wildcard)
    ["d/a/b/include/x.hrl",
     "d/a/b/src/x.erl",
     %% "g/c/d/include/y.hrl", Not expecting to find this one
     "g/c/f/include/z.hrl"] =
        run_emacs_support_escript_find_files(
          DirTop,
          filename:join([DirTop, "{d,g}/*/{b,f}/*/*.{erl,hrl}"])),
    ok.

setup_files(Files, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DirTop = mk_new_dir(PrivDir, "dir-", 1),
    setup_files_2(Files, DirTop),
    debug_dir_structure(DirTop),
    DirTop.

mk_new_dir(Dir, SubDirBase, I) ->
    Path = filename:join(Dir, SubDirBase ++ integer_to_list(I)),
    case file_make_dir(Path) of
        {error, eexist} ->
            mk_new_dir(Dir, SubDirBase, I+1);
        ok ->
            Path
    end.

setup_files_2([Entry | Rest], DirTop) ->
    case string:split(Entry, " -> ") of
        [_] ->
            Path = filename:join(DirTop, Entry),
            filelib_ensure_dir(Path),
            file_write_file(Path, <<>>),
            setup_files_2(Rest, DirTop);
        [SymlinkName, Target] ->
            Path = filename:join(DirTop, SymlinkName),
            filelib_ensure_dir(Path),
            file_make_symlink(Target, Path),
            setup_files_2(Rest, DirTop)
    end;
setup_files_2([], _DirTop) ->
    ok.

run_emacs_support_escript_find_files(TopDir, Pattern) ->
    Cmd = io_lib:format("escript '~s' find_files '~s'",
                        [emacs_support_escript(), Pattern]),
    Output = os:cmd(Cmd),
    Lines = [Line || Line <- string:lexemes(Output, ["\r\n", $\n])],
    case Lines of
        ["Result for "++_ | Results] ->
            lists:sort([unprefix_topdir(TopDir, RestLine)
                        || "  "++RestLine <- Results]);
        _ ->
            ct:log("Unexpected output from escript:~n~s~n---------~n",
                   [Output]),
            error({unexpected_output, Lines})
    end.

unprefix_topdir(TopDir, Path) ->
    case lists:prefix(TopDir, Path) of
        true  -> string:slice(Path, length(TopDir) + 1); % + 1 to skip the '/'
        false -> Path
    end.

%% Some wrappers for functions in the `file' module, to make debugging
%% easier in case of failures, so we see what path any failure is for.
file_make_dir(Path) ->
    case file:make_dir(Path) of
        ok              -> ok;
        {error, eexist} -> {error, eexist};
        Other           -> file_fail({'file:make_dir',[Path]}, Other)
    end.

file_write_file(Path, Data) ->
    case file:write_file(Path, Data) of
        ok           -> ok;
        {error, Why} -> file_fail({'file:make_dir',[Path]}, Why)
    end.

filelib_ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok  -> ok;
        Err -> file_fail({'filelib:ensure_dir',[Path]}, Err)
    end.

file_make_symlink(Target, Path) ->
    case file:make_symlink(Target, Path) of
        ok           -> ok;
        {error, Why} -> file_fail({'file:make_symlink',[Target, Path]}, Why)
    end.


file_fail(Call, Error) ->
    error({file_op_failed, #{call => Call,
                             error => Error}}).

debug_dir_structure(DirTop) ->
    LsOutput = os:cmd("find '" ++ DirTop ++ "' -print0 | xargs -0 ls -ld"),
    ct:log("dir structure in ~p:~n~s-------~n",
           [DirTop, string:replace(LsOutput, DirTop, "...", all)]).

emacs_support_escript() ->
    filename:join([emacs_dir(), "emacs_erlang_yaemep_support.erl"]).

emacs_cache_dir() ->
    Out = emacs("--eval \"(require 'yaemep)\"  --eval \"(message (yaemep-completion-cache-dir))\" "),
    Rows = string:lexemes(Out, ["\r\n", $\n]),
    lists:last(Rows).


emacs_var_completion_string() ->
    "\"f(Var1, Var2, Var3, [E1, E2]) ->\n"
        "{V1, V2} = Var1\n"
        "Fun = fun(In1, In2) -> In3 = hej, ok end\n"
        "Var4 = hej,\n"
        "true = lists:all(fun(Var5) ->\n"
        "                     lists:any(fun(Var6) ->\"".

parse_completion_list(Str) ->
    string:lexemes(Str, [$;]).


%% Test for complete at point


update_completion_cache() ->
    Command = io_lib:format("escript ~s update_completion_cache ~s ~s",
                            [emacs_support_escript(),
                             emacs_cache_dir(),
                             emacs_support_escript()]),
    os:cmd(Command).

test_complete_everything(_Config) ->
    Command = io_lib:format("escript ~s list_modules_and_functions_in_erl_file ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     emacs_var_completion_string()]),
    update_completion_cache(),
    {Time, Out} =
        timer:tc(fun() ->
                         os:cmd(Command)
                 end),
    Res = sets:from_list(parse_completion_list(Out)),
    true = lists:all(fun(ShouldBeThere) ->
                             case sets:is_element(ShouldBeThere, Res) of
                                 false -> false;
                                 true -> true
                             end
                     end,
                     ["Var1",
                      "Var2",
                      "Var3",
                      "E1",
                      "E2",
                      "V1",
                      "V2",
                      "Fun",
                      "Var4",
                      "Var5",
                      "Var6",
                      "lists:",
                      "sets:",
                      "maps:",
                      "crypto:",
                      "main(List)"]),
    true = lists:all(fun(ShouldNotBeThere) ->
                             not sets:is_element(ShouldNotBeThere, Res)
                     end,
                     ["In1",
                      "In2",
                      "In3"]),
    {comment, Time/1000000}.


test_complete_var(_Config) ->
    {Time, Out} =
        timer:tc(fun() ->
                         os:cmd(io_lib:format("escript ~s list_local_vars ~s ~s ~s",
                                              [emacs_support_escript(),
                                               emacs_cache_dir(),
                                               emacs_support_escript(),
                                               emacs_var_completion_string()]))
                 end),
    Res = sets:from_list(parse_completion_list(Out)),
    true = lists:all(fun(ShouldBeThere) ->
                             case sets:is_element(ShouldBeThere, Res) of
                                 false -> false;
                                 true -> true
                             end
                     end,
                     ["Var1",
                      "Var2",
                      "Var3",
                      "E1",
                      "E2",
                      "V1",
                      "V2",
                      "Fun",
                      "Var4",
                      "Var5",
                      "Var6"]),
    true = lists:all(fun(ShouldNotBeThere) ->
                             not sets:is_element(ShouldNotBeThere, Res)
                     end,
                     ["In1",
                      "In2",
                      "In3",
                      "lists:",
                      "sets:",
                      "maps:",
                      "crypto:",
                      "main(Arg)"]),
    {comment, Time/1000000}.


test_complete_local_function(_Config) ->
    {Time, Out} =
        timer:tc(fun() ->
                         os:cmd(io_lib:format("escript ~s list_functions_in_erl_file ~s ~s ~s",
                                              [emacs_support_escript(),
                                               emacs_cache_dir(),
                                               emacs_support_escript(),
                                               "not_used"]))
                 end),
    Res = sets:from_list(parse_completion_list(Out)),
    true = lists:all(fun(ShouldBeThere) ->
                             case sets:is_element(ShouldBeThere, Res) of
                                 false -> false;
                                 true -> true
                             end
                     end,
                     ["main(List)"]),
    true = lists:all(fun(ShouldNotBeThere) ->
                             not sets:is_element(ShouldNotBeThere, Res)
                     end,
                     ["Var2",
                      "Var1",
                      "In1",
                      "In2",
                      "In3",
                      "lists:",
                      "sets:",
                      "maps:",
                      "crypto:",
                      "main_not(Arg)"]),
    {comment, Time/1000000}.

test_complete_module(_Config) ->
    {Time, Out} =
        timer:tc(fun() ->
                         os:cmd(io_lib:format("escript ~s list_modules ~s ~s ~s",
                                              [emacs_support_escript(),
                                               emacs_cache_dir(),
                                               emacs_support_escript(),
                                               "not_used"]))
                 end),
    Res = sets:from_list(parse_completion_list(Out)),
    true = lists:all(fun(ShouldBeThere) ->
                             case sets:is_element(ShouldBeThere, Res) of
                                 false -> false;
                                 true -> true
                             end
                     end,
                     ["lists",
                      "sets",
                      "maps",
                      "crypto"]),
    true = lists:all(fun(ShouldNotBeThere) ->
                             not sets:is_element(ShouldNotBeThere, Res)
                     end,
                     ["Var2",
                      "Var1",
                      "In1",
                      "In2",
                      "In3",
                      "main_not(Arg)"]),
    {comment, Time/1000000}.


test_complete_functions_in_module(_Config) ->
    Times =
        [
         begin
             {Time, Out} =
                 timer:tc(fun() ->
                                  os:cmd(io_lib:format("escript ~s list_functions_in_module ~s ~s ~s",
                                                       [emacs_support_escript(),
                                                        emacs_cache_dir(),
                                                        emacs_support_escript(),
                                                        CompleteString]))
                          end),
             Res = sets:from_list(parse_completion_list(Out)),
             true = lists:all(fun(ShouldBeThere) ->
                                      case sets:is_element(ShouldBeThere, Res) of
                                          false -> false;
                                          true -> true
                                      end
                              end,
                              ["lists:foreach(A1, A2)",
                               "lists:all(A1, A2)",
                               "lists:concat(Arg)",
                               "lists:duplicate(A1, A2)"]),
             true = lists:all(fun(ShouldNotBeThere) ->
                                      not sets:is_element(ShouldNotBeThere, Res)
                              end,
                              ["lists:foreach"]),Time
         end
         || CompleteString <- ["lists:", "lists", "lists:for"]
        ],
    {comment, [T/1000000 || T <- Times]}.


test_complete_functions_in_module_from_erl_file(_Config) ->
    {Time, Out} =
        timer:tc(fun() ->
                         os:cmd(io_lib:format("escript ~s list_functions_in_module ~s ~s ~s",
                                              [emacs_support_escript(),
                                               emacs_cache_dir(),
                                               emacs_support_escript(),
                                               "emacs_erlang_yaemep_support"]))
                 end),
    Res = sets:from_list(parse_completion_list(Out)),
    true = lists:all(fun(ShouldBeThere) ->
                             case sets:is_element(ShouldBeThere, Res) of
                                 false -> false;
                                 true -> true
                             end
                     end,
                     ["emacs_erlang_yaemep_support:main(Arg)"]),
    true = lists:all(fun(ShouldNotBeThere) ->
                             not sets:is_element(ShouldNotBeThere, Res)
                     end,
                     ["lists:foreach"]),
    {comment, Time/1000000}.
