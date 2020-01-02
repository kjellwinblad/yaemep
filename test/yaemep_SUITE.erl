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
         test_compile_and_load/1
        ]).

all() ->
    [
     test_complete_everything,
     test_complete_var,
     test_complete_local_function,
     test_complete_module,
     test_complete_functions_in_module,
     test_complete_functions_in_module_from_erl_file,
     test_compile_and_load
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
    Res = sets:from_list(parse_completion_list(os:cmd(Command))),
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
                      "In3"]).


test_complete_var(_Config) ->
    Res = sets:from_list(parse_completion_list(os:cmd(io_lib:format("escript ~s list_local_vars ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     emacs_var_completion_string()])))),
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
                      "main(Arg)"]).


test_complete_local_function(_Config) ->
    Res = sets:from_list(parse_completion_list(os:cmd(io_lib:format("escript ~s list_functions_in_erl_file ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     "not_used"])))),
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
                      "main_not(Arg)"]).

test_complete_module(_Config) ->
    Res = sets:from_list(parse_completion_list(os:cmd(io_lib:format("escript ~s list_modules ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     "not_used"])))),
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
                      "main_not(Arg)"]).


test_complete_functions_in_module(_Config) ->
    [
     begin
         Res = sets:from_list(parse_completion_list(os:cmd(io_lib:format("escript ~s list_functions_in_module ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     CompleteString])))),
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
                     ["lists:foreach"])
     end
     || CompleteString <- ["lists:", "lists", "lists:for"]
    ].


test_complete_functions_in_module_from_erl_file(_Config) ->
    [
     begin
         io:format(io_lib:format("escript ~s list_functions_in_module ~s ~s ~s",
                                 [emacs_support_escript(),
                                  emacs_cache_dir(),
                                  emacs_support_escript(),
                                  CompleteString])),
         Res = sets:from_list(parse_completion_list(os:cmd(io_lib:format("escript ~s list_functions_in_module ~s ~s ~s",
                                                                    [emacs_support_escript(),
                                                                     emacs_cache_dir(),
                                                                     emacs_support_escript(),
                                                                     CompleteString])))),
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
                     ["lists:foreach"])
     end
     || CompleteString <- ["emacs_erlang_yaemep_support"]
    ].
