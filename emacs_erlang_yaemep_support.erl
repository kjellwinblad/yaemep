-module(emacs_erlang_yaemep_support).

-export([main/1]).

-spec erlang_project_dir(string()) -> string().
erlang_project_dir(ErlangFilePath) ->
    DirPath = filename:dirname(ErlangFilePath),
    SplittedDirPath = filename:split(DirPath),
    FinderDown =
        fun FinderDownFun([], _) -> false;
            FinderDownFun(Path, Predicate)->
                case Predicate(Path) of
                    true -> Path;
                    false ->
                        case lists:droplast(filename:split(Path)) =:= [] of
                            true -> false;
                            false ->
                                FinderDownFun(
                                  filename:join(lists:droplast(filename:split(Path))),
                                  Predicate)
                        end
                end
        end,
    FinderUp =
        fun FinderUpFun(PathList, [], Predicate) ->
                case Predicate(filename:join(PathList)) of
                    true -> filename:join(PathList);
                    false -> false
                end;
            FinderUpFun([], [ToAdd | Rest], Predicate) ->
                FinderUpFun([ToAdd], Rest, Predicate);
            FinderUpFun(PathListToCheck, [ToAdd | Rest], Predicate) ->
                case Predicate(filename:join(PathListToCheck)) of
                    true -> filename:join(PathListToCheck);
                    false ->
                        FinderUpFun(PathListToCheck ++ [ToAdd], Rest, Predicate)
                end
        end,
    case FinderUp([],
                  SplittedDirPath,
                  fun(Path) ->
                          lists:any(
                            fun(F) ->
                                    filelib:is_file(filename:join(Path, F))
                            end,
                            [".emacs_erlang_mode_project",
                             "rebar.config",
                             "mix.exs",
                             "erlang.mk"])
                  end) of
        false ->
            FinderDown(DirPath,
                       fun(Path) ->
                               lists:any(
                                 fun(F) ->
                                         filelib:is_dir(filename:join(Path, F))
                                 end,
                                 [".git",
                                  ".svn",
                                  ".hg",
                                  ".cvs"])
                       end);
        Path -> Path
    end.

%% Detect is a Erlang/OTP source code directory
is_erlang_otp_src_dir(Dir) ->
    filelib:is_dir(filename:join(Dir, "lib")) andalso
        filelib:is_dir(filename:join(Dir, "erts")) andalso
        lists:all(fun(Lib) ->
                          filelib:is_dir(filename:join([Dir, "lib", Lib]))
                  end, ["compiler", "kernel", "stdlib"]).

-spec update_etags(string(),string(),string(),[string()]) -> ok.
update_etags(ProjectDir, TagsFileName, SearchPattern, AdditionalDirectories) ->
    Dirs1 = [ProjectDir | AdditionalDirectories],
    %% Include only the first Erlang/OTP directory
    Dirs2 =
        lists:reverse(
          erlang:element(
            2,
            lists:foldl(
              fun(Dir, {false, SoFar}) ->
                      case is_erlang_otp_src_dir(Dir) of
                          true -> {true, [Dir|SoFar]};
                          false -> {false, [Dir|SoFar]}
                      end;
                 (Dir, {true, SoFar}) ->
                      case is_erlang_otp_src_dir(Dir) of
                          true -> {true, SoFar};
                          false -> {true, [Dir|SoFar]}
                      end
              end,
              {false, []},
              Dirs1))),
    ErlHrlFiles1 =
        lists:foldl(
          fun(Directory, SoFar) ->
                  filelib:wildcard(filename:join(Directory, SearchPattern)) ++ SoFar
          end,
          [],
          Dirs2),
    %% Ugly hack to filter out files under release/tests in otp folder
    ErlHrlFiles2 =
        case lists:search(fun (D) -> is_erlang_otp_src_dir(D) end, Dirs2) of
            false ->
                ErlHrlFiles1;
            {value, OtpDir} ->
                lists:filter(
                 fun(Path) ->
                         FilterOutPattern =
                             filename:join([OtpDir, "release", "tests"]),
                         case string:find(Path, FilterOutPattern)
                         of
                             nomatch -> true;
                             M -> not (erlang:iolist_to_binary(M) =:=
                                           erlang:iolist_to_binary(Path))
                         end
                 end,
                 ErlHrlFiles1)
            end,
    os:cmd(io_lib:format("etags -o ~s ~s",
                         [TagsFileName, lists:join(" ", ErlHrlFiles2)])),
    ok.


-spec update_etags(string(),string(),string()) -> ok.
update_etags(ProjectDir, TagsFileName, SearchPattern) ->
    update_etags(ProjectDir, TagsFileName, SearchPattern, []).


-spec update_etags(string(),string()) -> ok.
update_etags(ProjectDir, TagsFileName) ->
    update_etags(ProjectDir, TagsFileName, "**/*.{erl,hrl}").


-spec update_etags(string()) -> ok.
update_etags(ProjectDir) ->
    update_etags(ProjectDir, "TAGS").


-spec erlang_project_lib_directories(string()) -> [string()].
erlang_project_lib_directories(ProjectDir) ->
    BeamFiles = filelib:wildcard(filename:join(ProjectDir, "**/*.beam")),
    BeamDirs = [filename:dirname(BeamFile) || BeamFile <- BeamFiles],
    sets:to_list(sets:from_list(BeamDirs)).


-spec erlang_project_libs_parameter(string()) -> string().
erlang_project_libs_parameter(ProjectDir) ->
    LibDirs = erlang_project_lib_directories(ProjectDir),
    io_lib:format("-pa ~s", [lists:join(" ", LibDirs)]).


-spec erlang_project_add_project_lib_dirs_to_path(string()) -> ok.
erlang_project_add_project_lib_dirs_to_path(ProjectDir) ->
    LibDirs = erlang_project_lib_directories(ProjectDir),
    code:add_paths(LibDirs),
    ok.


-spec erlang_project_add_project_lib_dirs_to_path_from_cache(string(), string()) -> ok.
erlang_project_add_project_lib_dirs_to_path_from_cache(CacheDir, ProjectDir) ->
    LibsCacheFile =
        erlang_project_lib_directories_cache_file(CacheDir, ProjectDir),
    case filelib:is_file(LibsCacheFile) of
        true ->
            {ok, [LibDirs]} = file:consult(LibsCacheFile),
            code:add_paths(LibDirs);
        false ->
            erlang_project_add_project_lib_dirs_to_path(ProjectDir)
    end,
    ok.


-spec erlang_project_all_modules(string()) -> [string()].
erlang_project_all_modules(ProjectDir) ->
    erlang_project_add_project_lib_dirs_to_path(ProjectDir),
    BeamSet =
        sets:from_list([filename:rootname(filename:basename(F))
                        || P <- code:get_path(),
                           F <- filelib:wildcard(P ++ "/*.beam")]),
    ErlSet =
        sets:from_list(
          [filename:rootname(filename:basename(F))
           || F <- filelib:wildcard(filename:join(ProjectDir, "**/*.erl")),
              case re:run(filename:basename(F), "^[A-Za-z]") of
                  {match, _} -> true;
                  _ -> false
              end]),
    sets:to_list(sets:union(BeamSet, ErlSet)).


-spec erlang_project_cache_dir(string(), string()) -> string().
erlang_project_cache_dir(CacheDir, ProjectDir) ->
    ProjectDirNames = tl(filename:split(ProjectDir)),
    filename:join(filename:split(CacheDir) ++ ProjectDirNames).


-spec erlang_project_all_modules_cache_file(string(), string()) -> string().
erlang_project_all_modules_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "all_modules.txt").


-spec erlang_project_libs_parameter_cache_file(string(), string()) -> string().
erlang_project_libs_parameter_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "erl_lib_param.txt").


-spec erlang_project_lib_directories_cache_file(string(), string()) -> string().
erlang_project_lib_directories_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "erl_lib_dirs.txt").


-spec path_to_hex_hash_string(string()) -> string().
path_to_hex_hash_string(FileNameStr)->
    Hash = crypto:hash(sha256, erlang:iolist_to_binary(FileNameStr)),
    lists:flatten(io_lib:format(
                    "~s",
                    [lists:flatten([io_lib:format("~2.16.0B",[X])
                                    || <<X:8>> <= Hash ])])).


-spec functions_in_erl_file_cache_file(string(), string(), string()) -> string().
functions_in_erl_file_cache_file(CacheDir, ProjectDir, FileNameStr) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir,
                  io_lib:format("~s_funs_in_erl_file_cache.txt",
                                [path_to_hex_hash_string(FileNameStr)])).


-spec erlang_project_update_cache(string(), string(), string()) -> ok.
erlang_project_update_cache(CacheDir, ProjectDir, FileNameStr) ->
    AllModulesFile = erlang_project_all_modules_cache_file(CacheDir, ProjectDir),
    filelib:ensure_dir(AllModulesFile),
    file:write_file(AllModulesFile,
                    lists:join(";", erlang_project_all_modules(ProjectDir))),
    LibsParamCacheFile =
        erlang_project_libs_parameter_cache_file(CacheDir, ProjectDir),
    file:write_file(LibsParamCacheFile,
                    erlang_project_libs_parameter(ProjectDir)),
    LibDirsCacheFile =
        erlang_project_lib_directories_cache_file(CacheDir, ProjectDir),
    file:write_file(LibDirsCacheFile,
                    io_lib:format("~tp.~n",
                                  [erlang_project_lib_directories(ProjectDir)])),
    FunsInErlFileCacheFile =
        functions_in_erl_file_cache_file(CacheDir, ProjectDir, FileNameStr),
    file:write_file(FunsInErlFileCacheFile,
                    io_lib:format("~tp.~n",
                                  [list_functions_in_erl_file(FileNameStr)])),
    ok.


-spec erlang_project_all_modules_list(string(), string()) -> [string()].
erlang_project_all_modules_list(CacheDir, FileNameStr) ->
    ProjectDir = erlang_project_dir(FileNameStr),
    AllModulesFile =
        erlang_project_all_modules_cache_file(CacheDir, ProjectDir),
    case filelib:is_file(AllModulesFile) of
        true ->
            {ok, BinStr} = file:read_file(AllModulesFile),
            string:lexemes(BinStr, ";");
        false ->
            [atom_to_list(Module) || {Module, _Path} <- code:all_loaded()]
    end.


-spec mk_empty_parameter_list_string(number()) -> string().
mk_empty_parameter_list_string(0) ->
    "()";
mk_empty_parameter_list_string(1) ->
    "(Arg)";
mk_empty_parameter_list_string(Arity) ->
    lists:flatten("(" ++ lists:join(", ",
                                    [io_lib:format("A~p", [A])
                                     || A <- lists:seq(1, Arity)]) ++ ")").


list_functions_in_module_from_erl_file(_CacheDir,
                                       ProjectDir,
                                       ModuleNameStr) ->
    ErlFiles =
        filelib:wildcard(filename:join(ProjectDir,
                                       io_lib:format("**/~s.erl",
                                                     [ModuleNameStr]))),
    case ErlFiles of
        [ErlFile|_] ->
            {ok, BinStr} = file:read_file(ErlFile),
            {ok, RE} =
                re:compile("^[ \t\n]*-export[ \t\n]*\\([ \t\n]*\\[[ \t\n]*((?:(?:[a-zA-Z_0-9]+)/(?:[0-9]+)[\n]?[ \t\n]*,?[ \t\n]*)+)[ \t\n]*\\][ \t\n]*\\)[ \t\n]*\.",
                           [multiline, unicode]),
            case re:run(BinStr, RE, [global]) of
                {match, MatchList} ->
                    MatchList2 = [erlang:hd(erlang:tl(M)) || M <- MatchList],
                    MatchList3 = [binary:part(BinStr, Start, Length)
                                  || {Start, Length} <- MatchList2],
                    MatchList4 =
                        lists:foldl(
                          fun(E, SoFar) ->
                                  string:split(E, ",", all) ++ SoFar
                          end, [], MatchList3),
                    MatchList5 =
                        [(fun(S) ->
                                  case string:split(S, "/") of
                                      [Name,Arity] ->
                                          {string:trim(Name),
                                           erlang:element(1,
                                                          string:to_integer(Arity))};
                                      _ -> {"none", 0}
                                  end
                          end)(Str) || Str <- MatchList4],
                    MatchList5;
                _ -> []
            end;
        _ -> []
    end.


-spec list_functions_in_module(string(), string(), string()) -> [string()].
list_functions_in_module(CacheDir, FileNameStr, ModuleNameStr) ->
    ProjectDir = erlang_project_dir(FileNameStr),
    erlang_project_add_project_lib_dirs_to_path_from_cache(CacheDir,
                                                           ProjectDir),
    ModuleName = list_to_atom(ModuleNameStr),
    try
        lists:map(fun({FunName, Arity}) ->
                          io_lib:format(
                            "~s:~s~s",
                            [ModuleName,
                             FunName,
                             mk_empty_parameter_list_string(Arity)])
                  end,
                  proplists:get_value(exports, ModuleName:module_info()))
    catch
        _:_ ->
            lists:map(fun({FunName, Arity}) ->
                              io_lib:format(
                                "~s:~s~s",
                                [ModuleName,
                                 FunName,
                                 mk_empty_parameter_list_string(Arity)])
                      end,
                      list_functions_in_module_from_erl_file(CacheDir,
                                                             ProjectDir,
                                                             ModuleNameStr))
    end.


-spec list_functions_in_erl_file(string()) -> [string()].
list_functions_in_erl_file(FileNameStr) ->
    {ok, Data} = file:read_file(FileNameStr),
    Res = re:run(Data, "^[a-z0-9_]+\\s*\\([^\\)]*\\)",
                 [dotall, global, multiline, unicode]),
    UntilNoChange =
        fun UntilNoChange(D, Fun, N) ->
                After = Fun(D, N),
                case After =:= D of
                    true -> D;
                    false -> UntilNoChange(After, Fun, N + 1)
                end
        end,
    {ok, RemoveListRe} =
        re:compile("\\[[^\\]\\[]*\\]", [unicode]),
    RemoveList =
        fun(S,_) -> re:replace(S, RemoveListRe, "@", [global]) end,
    {ok, RemoveTupleRe} =
        re:compile("\\{[^\\}\\{]*\\}", [unicode]),
    RemoveTuple =
        fun(S,_) -> re:replace(S, RemoveTupleRe, "@", [global]) end,
    {ok, RemoveStringRe} =
        re:compile("\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"", [unicode]),
    RemoveString =
        fun(S,_) -> re:replace(S, RemoveStringRe, "@", [global]) end,
    {ok, RemoveWhiteSpaceRe} =
        re:compile("[ \\t\n=]", [unicode]),
    RemoveWhiteSpace =
        fun(S,_) -> re:replace(S, RemoveWhiteSpaceRe, "", [global]) end,
    {ok, RemoveNumberRe} =
        re:compile("[\\d]+\\.?[\\d]*", [unicode]),
    RemoveNumber =
        fun(S,_) -> re:replace(S, RemoveNumberRe, "@", [global]) end,
    {ok, RemoveCharEtcRe} =
        re:compile("((\\$.)|_|(#[a-zA-Z_0-9]+))", [unicode]),
    RemoveCharEtc =
        fun(S,_) -> re:replace(S, RemoveCharEtcRe, "@", [global]) end,
    {ok, RemoveSpecialRe} =
        re:compile("@+", [unicode]),
    RemoveSpecial =
        fun(S,N) -> re:replace(S, RemoveSpecialRe, io_lib:format("A~p", [N])) end,
    {ok, RemoveAtomsRe} = re:compile("(([a-z_0-9]+)|('.+'))", [unicode]),
    RemoveAll =
        fun(S,N) ->
                RemoveWhiteSpace(RemoveCharEtc(RemoveNumber(RemoveTuple(RemoveList(RemoveString(S, N), N+1),N+2),N+3), N+4), N+5)
        end,
    case Res of
        {match, ResList} ->
            FunMap =
                lists:foldl(
                  fun ([{Start, Length}], Map) ->
                          Head0 = binary:part(Data, Start, Length),
                          Head1 = string:replace(Head0, "\n", ""),
                          Head2 = string:replace(Head1, " ", ""),
                          Params0 = string:find(Head2, "(", leading),
                          FunctionName = string:trim(string:replace(Head2, Params0, "")),
                          Params1 = string:slice(Params0, 1, string:length(Params0) -2),
                          Params2 = UntilNoChange(Params1, RemoveAll, 1),
                          ParamsSplitted = string:lexemes(Params2, ","),
                          Params3 =
                              lists:join(
                                ",",
                                [(fun() ->
                                          Len = string:length(P),
                                          P2 = case re:run(P, RemoveSpecialRe, []) of
                                                   nomatch -> P;
                                                   {match,[{0,Len}]} -> "@";
                                                   _ -> re:replace(P, RemoveSpecialRe, "")
                                               end,
                                          case re:run(P2, RemoveAtomsRe) of
                                              nomatch -> P2;
                                              {match,[{0,Len}|_]} -> "@";
                                              _ -> P2
                                          end
                                  end)() || P <- ParamsSplitted]),
                          Arity = erlang:length(ParamsSplitted),
                          Params4 = UntilNoChange(Params3, RemoveSpecial, 1),
                          Params5 = case Arity of
                                        1 -> re:replace(Params4, "A1", "Arg");
                                        _ -> Params4
                                    end,
                          maps:put({binary:bin_to_list(erlang:iolist_to_binary(FunctionName)),
                                    Arity},
                                   re:replace(Params5, ",", ", ", [global]), Map)
                  end, #{}, ResList),
            [io_lib:format("~s(~s)", [Name, maps:get({Name, Arity}, FunMap)])
             || {Name, Arity} <- maps:keys(FunMap)];
        _ -> []
    end.

-spec list_functions_in_erl_file_from_cache(string(), string()) -> [string()].
list_functions_in_erl_file_from_cache(CacheDir, FileNameStr) ->
    ProjectDir = erlang_project_dir(FileNameStr),
    FunsInErlFileCacheFile =
        functions_in_erl_file_cache_file(CacheDir, ProjectDir, FileNameStr),
    case filelib:is_file(FunsInErlFileCacheFile) of
        true ->
            {ok, [FunctionList]} = file:consult(FunsInErlFileCacheFile),
            FunctionList;
        false ->
            list_functions_in_erl_file(FileNameStr)
    end.

-spec remove_irrelevant_scopes(string()) -> string().
remove_irrelevant_scopes(TextP) ->
    Text = erlang:iolist_to_binary(TextP),
    NonClosulreGroupStarts =
        case re:run(Text, "(?|begin)|(?|case)|(?|if)|(?|receive)|(?|try)",
                    [global, unicode]) of
            {match, List} -> maps:from_list(lists:flatten(List)) ;
            _ -> #{}
        end,
    ClosureGroupStarts =
        case re:run(Text, "(?|fun)", [global, unicode]) of
            {match, List2} -> maps:from_list(lists:flatten(List2));
            _ -> #{}
        end,
    Ends = case re:run(Text, "(?|end)", [global, unicode]) of
               {match, List3} -> maps:from_list(lists:flatten(List3));
               _ -> #{}
           end,
    GetToRemoveRanges =
        fun GTRM([], _, _, RemoveRanges) -> RemoveRanges;
            GTRM([_|Rest], Pos, OpenedThings, RemoveRangesSoFar) ->
                case {maps:get(Pos, NonClosulreGroupStarts, false),
                      maps:get(Pos, ClosureGroupStarts, false),
                      maps:get(Pos, Ends, false)} of
                    {false, false, false} ->
                        GTRM(Rest, Pos+1, OpenedThings, RemoveRangesSoFar);
                    {_, false, false} ->
                        GTRM(Rest, Pos+1, [non_c|OpenedThings], RemoveRangesSoFar);
                    {false, _, false} ->
                        GTRM(Rest, Pos+1, [Pos|OpenedThings], RemoveRangesSoFar);
                    {false, false, _} ->
                        case OpenedThings of
                            [] ->
                                GTRM(Rest, Pos+1, [], RemoveRangesSoFar);
                            [non_c|OpenedThingsRest] ->
                                GTRM(Rest,
                                     Pos+1,
                                     OpenedThingsRest,
                                     RemoveRangesSoFar);
                            [StartPos|OpenedThingsRest] ->
                                GTRM(Rest, Pos+1,
                                     OpenedThingsRest,
                                     [{StartPos,Pos}|RemoveRangesSoFar])
                        end
                end
        end,
    CharsInText = unicode:characters_to_list(Text),
    RemoveRanges1 = GetToRemoveRanges(CharsInText, 0, [], []),
    RemoveRanges2 =
        lists:filter(
          fun({Start, End}) ->
                  not lists:any(
                        fun({OStart, OEnd}) ->
                                OStart < Start andalso OEnd > End
                        end, RemoveRanges1)
          end, RemoveRanges1),
    lists:foldl(
      fun({Start, End}, TextSoFar) ->
              string:replace(TextSoFar,
                             binary:part(Text, Start, End - Start),
                             "")
      end, Text, RemoveRanges2).

-spec list_local_vars(string()) -> [string()].
list_local_vars(CompletionString) ->
    N1 = re:replace(CompletionString,
                    "\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"", "",
                    [global, unicode]),
    N2 = re:replace(N1, "[%].*", "", [global, unicode]),
    N3 = erlang:iolist_to_binary(remove_irrelevant_scopes(N2)),
    Res = re:run(N3, "[A-Z][A-Za-z0-9_]*",
                 [dotall, global, multiline, unicode]),
    sets:to_list(
      sets:from_list(
        case Res of
            nomatch -> [];
            {match, List} ->
                [erlang:iolist_to_binary(binary:part(N3, Start, Length))
                 || [{Start, Length}] <- List]
        end)).

-spec wrapped_main([string()]) -> ok.
wrapped_main(["check"]) ->
    io:format("OK");
wrapped_main(["get_project_dir", FileNameStr]) ->
    io:format(erlang_project_dir(FileNameStr));
wrapped_main(["update_etags", FileNameStr]) ->
    update_etags(erlang_project_dir(FileNameStr));
wrapped_main(["update_etags_project_dir",
              ProjectDir,
              TagsFileName,
              SearchPattern|
              AdditionalDirs]) ->
    update_etags(ProjectDir, TagsFileName, SearchPattern, AdditionalDirs);
wrapped_main(["update_completion_cache", CacheDir, FileNameStr]) ->
    erlang_project_update_cache(CacheDir,
                                erlang_project_dir(FileNameStr),
                                FileNameStr);
wrapped_main(["list_modules", CacheDir, FileNameStr, _]) ->
    io:format(
      lists:join(
        ";",
        erlang_project_all_modules_list(CacheDir, FileNameStr)));
wrapped_main(["list_functions_in_module",
              CacheDir,
              FileNameStr,
              CompletionString]) ->
    ModuleNameStr = case string:lexemes(CompletionString, ":") of
                        [X] -> X;
                        [X,_] -> X
                    end,
    io:format("~s",
              [lists:join(";",
                          list_functions_in_module(CacheDir,
                                                   FileNameStr,
                                                   ModuleNameStr))]);
wrapped_main(["list_functions_in_erl_file",
              CacheDir,
              FileNameStr,
              _CompletionString]) ->
    io:format("~s",
              [lists:join(
                 ";",
                 list_functions_in_erl_file_from_cache(CacheDir,
                                                       FileNameStr))]);
wrapped_main(["list_modules_and_functions_in_erl_file",
              CacheDir,
              FileNameStr,
              CompletionString]) ->
    Modules = [[M, ":"]
               || M <- erlang_project_all_modules_list(CacheDir, FileNameStr)],
    LocalFunctions =
        list_functions_in_erl_file_from_cache(CacheDir, FileNameStr),
    Vars = list_local_vars(CompletionString),
    Res = lists:join(";",
                     [lists:join(";", LocalFunctions),
                      lists:join(";", Modules),
                      lists:join(";", Vars)]),
    io:format("~s", [Res]);
wrapped_main(["list_local_vars", _CacheDir, _FileNameStr, CompletionString]) ->
    io:format("~s", [lists:join(";", list_local_vars(CompletionString))]).

-spec main([string()]) -> ok.
main(List) ->
    wrapped_main(List).



