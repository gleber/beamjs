-module(beamjs_mod_require).

-export([exports/1, init/1, require/2]).

-behaviour(beamjs_module).

-include_lib("erlv8/include/erlv8.hrl").

init(VM) ->
    erlv8_vm:stor(VM, {beamjs_mod_require, mod_tab},
                  ets:new(require_modules, [public, {heir, VM, none}])),
    ok.

exports(VM) ->
    {ok, Cwd} = file:get_cwd(),
    Paths0 = case os:getenv("NODE_PATH") of
                 false -> [Cwd];
                 NodePath -> [Cwd, NodePath]
             end,
    Paths = erlv8_vm:taint(VM, ?V8Arr(Paths0)),
    Paths:set_value("__doc__",
                    "Array of paths where require() will be looking for modules"),
    erlv8_fun:new(fun require_fun/2,
                  ?V8Obj([{"__doc__",
                           "`require()` provides a way to load another javascript "
                           "modules. Not fully compatible with [CommonJS Modules/1.1](htt"
                           "p://wiki.commonjs.org/wiki/Modules/1.1) yet, but eventually "
                           "[will be](https://github.com/beamjs/beamjs/issues/issue/3)."
                           ++ [10] ++ "### Synopsis\n        require(ModuleId)    "},
                          {"paths", Paths}])).

require(VM, Filename) when is_pid(VM), is_atom(Filename) ->
    erlv8_vm:taint(VM, Filename:exports(VM));
require(VM, Filename) when is_pid(VM) ->
    {_, Ctx} = erlv8_context:get(VM),
    require_fun(#erlv8_fun_invocation{vm = VM, ctx = Ctx}, [Filename]).

require_fun(#erlv8_fun_invocation{vm = VM}, [#erlv8_object{} = Opts]) ->
    PL = Opts:proplist(),
    case {proplists:get_value(<<"module">>, PL),
          proplists:get_value(<<"join">>, PL)} of
        {Module, _} when is_binary(Module) ->
            Mod = list_to_atom(binary_to_list(Module)),
            erlv8_vm:taint(VM, Mod:exports(VM));
        {_, Modules} when is_list(Modules) ->
            NewExports = erlv8_vm:taint(VM, ?V8Obj([])),
            Throws = [V2 || V2 <- [do_require_fun(V1, NewExports, VM) || V1 <- Modules],
                            require_fun_check(V2)],
            case Throws of
                [Throw | _] -> Throw;
                _ -> NewExports
            end;
        _ -> {throw, "Unknown key"}
    end;
require_fun(#erlv8_fun_invocation{vm = VM} = Invocation, [Filename]) ->
    case require_module(Invocation, Filename) of
        {throw, E} ->
            case erlv8_vm:retr(VM, {beamjs_bundle, module, Filename}) of
                undefined -> {throw, E};
                #erlv8_object{} = O -> O
            end;
        Exports -> Exports
    end.

do_require_fun(Module, NewExports, VM) ->
    Exports = require(VM, Module),
    Exports:copy_properties_to(NewExports).

require_fun_check({throw, _}) -> true;
require_fun_check(_) -> false.

%% file_reader(Path, Filename) ->
%%     case file_reader(Path, Filename, ".js") of
%%         not_found -> file_reader(Path, Filename, "");
%%         Result -> Result
%%     end.

file_reader(Path0, Filename0, Ext) ->
    Path = binary_to_list(iolist_to_binary(Path0)),
    Filename = binary_to_list(iolist_to_binary(Filename0)),
    file_reader0(filename:join([Path, Filename ++ Ext])).

file_reader0(Fullfilename) ->
    case file:read_file(Fullfilename) of
        {error, _} -> not_found;
        {ok, B} ->
            {filename:dirname(filename:absname(Fullfilename)),
             filename:basename(Fullfilename),
             binary_to_list(B)}
    end.

require_module(#erlv8_fun_invocation{vm = VM, ctx = _Ctx} = Invocation, Filename) ->
    Global = Invocation:global(),
    Require = Global:get_value("require"),
    Paths = Require:get_value("paths", erlv8_vm:taint(VM, ?V8Arr([]))),
    Module = Global:get_value("module"),
    ModuleId = case Module of
                   undefined -> undefined;
                   _ -> Module:get_value("id")
               end,
    Dirname = Require:get_value("__dirname"),
    AllPaths = [Dirname | Paths:list()],
    Require:set_value("paths", Paths),
    Sources = [V2 || V2 <- [do_require_module(V1, Filename) || V1 <- AllPaths],
                     require_file_check(V2)],
    case Sources of
        [] ->
            AllPathsNice = [ [binary_to_list(X), ", \n"] || X <- AllPaths ],
            {throw,
             {error, lists:flatten(io_lib:format("Cannot find module '~s' in paths:~n~s", [Filename, AllPathsNice]))}};
        [{Path, LoadedFilename, S} | _] ->
            Tab = erlv8_vm:retr(VM, {beamjs_mod_require, mod_tab}),
            case ets:lookup(Tab, Filename) of
                [{Filename, loading}] ->
                    ModuleId = Module:get_value("id"),
                    ets:delete(Tab, Filename),
                    ets:delete(Tab, ModuleId),
                    {throw,
                     {error,
                      lists:flatten(io_lib:format("Dependency cycle detected while attempting to load '~s' "
                                                  "from '~s'",
                                                  [Filename, ModuleId]))}};
                [{Filename, Exports}] -> Exports;
                [] ->
                    %% prepare wrapping function
                    Script = iolist_to_binary(["(function(require, module, __dirname, __filename) {"
                                               "   var exports = {};",
                                               S,
                                               "   ;return exports;",
                                               "})"]),
                    %% add module-specific require with require.main
                    NewRequire = erlv8_vm:taint(VM, fun require_fun/2),
                    NewRequire:set_value("main", Require:get_value("main")),
                    NewPath = ?V8Arr((lists:usort([Path, Dirname | Paths:list()]))),
                    NewRequire:set_value("paths", NewPath, [dontdelete, readonly]),
                    NewRequire:set_value("__dirname", Path, [dontdelete, readonly]),
                    Require:set_value("__dirname", Path, [dontdelete, readonly]),
                    Require:set_value("paths", NewPath),

                    %% module.id and module.url
                    NewModule = case ModuleId of
                                    Filename -> Module;
                                    _ ->
                                        NM = erlv8_vm:taint(VM, ?V8Obj([])),
                                        NM:set_value("id", Filename, [dontdelete, readonly]),
                                        NM:set_value("url", Path, [dontdelete, readonly])
                                end,

                    ets:insert(Tab, {Filename, loading}),

                    case erlv8_vm:run(VM, erlv8_context:get(VM), Script, {LoadedFilename, 0, 0}) of
                        {ok, Wrapped} ->
                            case Wrapped:call([NewRequire, NewModule, Path, Filename]) of
                                Exports when ?is_v8(Exports) ->
                                    ets:insert(Tab, {Filename, Exports}),
                                    Exports;
                                {_, {error, E}} ->
                                    ets:delete(Tab, Filename),
                                    {throw, {error, E}}
                            end;
                        {_, {error, E}} ->
                            ets:delete(Tab, Filename),
                            {throw, {error, E}}
                    end
            end
    end.

do_require_module(Path, Module) ->
    %% Files to check:
    %% 1. module.js
    %% 2. module
    %% 3. module/{read(package.json)[main]}
    %% 4. module/index.js
    try_load([
              fun() ->
                      file_reader(Path, Module, ".js")
              end,
              fun() ->
                      file_reader(Path, Module, "")
              end,
              fun() ->
                      package_json_reader(Path, Module)
              end,
              fun() ->
                      file_reader(filename:join([Path, Module]), "index", ".js")
              end
             ]).

package_json_reader(Path, Package) ->
    case file:read_file(filename:join([Path, Package, "package.json"])) of
        {error, _} ->
            not_found;
        {ok, Data} ->
            try
                Manifest = jsx:decode(Data),
                case proplists:get_value(<<"main">>, Manifest) of
                    undefined -> not_found;
                    Filename ->
                        case file_reader0(filename:join([Path, Package, Filename])) of
                            not_found ->
                                file_reader0(filename:join([Path, Package, binary_to_list(Filename)++".js"]));
                            Res ->
                                Res
                        end
                end
            catch
                _:_ ->
                    not_found
            end
    end.

try_load([]) -> not_found;
try_load([F|T]) ->
    case erlang:apply(F, []) of
        not_found -> try_load(T);
        R         -> R
    end.

require_file_check(not_found) -> false;
require_file_check(_) -> true.
