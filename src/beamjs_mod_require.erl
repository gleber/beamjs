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
    Paths = erlv8_vm:taint(VM, ?V8Arr([Cwd])),
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
            Throws = [V2
                      || V2 <- [require_fun_1(V1, NewExports, VM) || V1 <- Modules],
                         require_fun_1(V2)],
            case Throws of
                [Throw | _] -> Throw;
                _ -> NewExports
            end;
        _ -> {throw, "Unknown key"}
    end;
require_fun(#erlv8_fun_invocation{vm = VM} = Invocation, [Filename]) ->
    case require_file(Invocation, Filename) of
        {throw, E} ->
            case erlv8_vm:retr(VM, {beamjs_bundle, module, Filename}) of
                undefined -> {throw, E};
                #erlv8_object{} = O -> O
            end;
        Exports -> Exports
    end.

require_fun_1(Module, NewExports, VM) ->
    Exports = require(VM, Module),
    lists:foreach(fun ({K, V}) -> NewExports:set_value(K, V) end,
                  Exports:proplist()).

require_fun_1({throw, _}) -> true;
require_fun_1(_) -> false.

file_reader(Path, Filename) ->
    case file_reader(Path, Filename, ".js") of
        not_found -> file_reader(Path, Filename, "");
        Result -> Result
    end.

file_reader(Path0, Filename0, Ext) ->
    Path = binary_to_list(iolist_to_binary(Path0)),
    Filename = binary_to_list(iolist_to_binary(Filename0)),
    case file:read_file(filename:join([Path, Filename ++ Ext])) of
        {error, _} -> not_found;
        {ok, B} ->
            {filename:dirname(filename:absname(Path ++ "/" ++ Filename)), Filename ++ Ext,
             binary_to_list(B)}
    end.

require_file(#erlv8_fun_invocation{vm = VM, ctx = _Ctx} = Invocation, Filename) ->
    Global = Invocation:global(),
    Require = Global:get_value("require"),
    Paths = Require:get_value("paths", erlv8_vm:taint(VM, ?V8Arr([""]))),
    Require:set_value("paths", Paths),
    Sources = [V2 || V2 <- [do_require_file(V1, Filename) || V1 <- Paths:list()],
                     require_file_check(V2)],
    %% io:format("~s sources ~p~n", [?MODULE, Sources]),
    case Sources of
        [] ->
            {throw,
             {error, lists:flatten(io_lib:format("Cannot find module '~s'", [Filename]))}};
        [{Path, LoadedFilename, S} | _] ->
            Tab = erlv8_vm:retr(VM, {beamjs_mod_require, mod_tab}),
            case ets:lookup(Tab, Filename) of
                [{Filename, loading}] ->
                    Module = Global:get_value("module"),
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
                    Script = iolist_to_binary(["(function(require, module) {"
                                               "   var exports = {};",
                                               S,
                                               "   ;return exports;",
                                               "})"]),
                    %% add module-specific require with require.main
                    NewRequire = erlv8_vm:taint(VM, fun require_fun/2),
                    NewRequire:set_value("main", Require:get_value("main"), [dontdelete, readonly]),
                    NewRequire:set_value("paths",
                                         ?V8Arr((lists:usort(Paths:list()))),
                                         [dontdelete, readonly]),
                    %% module.id and module.url
                    Module = erlv8_vm:taint(VM, ?V8Obj([])),
                    Module:set_value("id", Filename, [dontdelete, readonly]),
                    Module:set_value("url", Path, [dontdelete, readonly]),
                    ets:insert(Tab, {Filename, loading}),
                    %% io:format("Requiring: ~p~n", [Script]),
                    case erlv8_vm:run(VM, erlv8_context:get(VM), Script, {LoadedFilename, 0, 0}) of
                        {ok, Wrapped} ->
                            %% io:format("Wrapped: ~p~n", [erlv8_vm:to_detail_string(VM,Wrapped)]),
                            case Wrapped:call([NewRequire, Module]) of
                                {ok, Exports} ->
                                    ets:insert(Tab, {Filename, Exports}),
                                    Exports;
                                {_, {error, E}} ->
                                    %% io:format("Calling error: ~p~n", [E:proplist()]),
                                    ets:delete(Tab, Filename),
                                    {throw, {error, E}}
                            end;
                        {_, {error, E}} ->
                            %% io:format("Wrapping error: ~p~n", [E:proplist()]),
                            ets:delete(Tab, Filename),
                            {throw, {error, E}}
                    end
            end
    end.

do_require_file(Path, Filename) -> file_reader(Path, Filename).

require_file_check(not_found) -> false;
require_file_check(_) -> true.
