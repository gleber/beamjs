-module(beamjs).

-include_lib("erlv8/include/erlv8.hrl").

-export([main/0, start/0, stop/0]).

start() -> application:start(beamjs).

stop() -> application:stop(beamjs).

%%%

install_require(VM) ->
    Global = erlv8_vm:global(VM),
    beamjs_mod_require:init(VM),
    Global:set_value("require", beamjs_mod_require:exports(VM)).

%%%

args(preemption) ->
    case init:get_argument(jspreemption) of
        {ok, [[V | _]]} ->
            application:set_env(erlv8, preemption_ms, list_to_integer(V));
        _ -> false
    end;
args(norepl) ->
    case init:get_argument(norepl) of
        {ok, _} -> true;
        _ -> false
    end;
args(toolbar) ->
    case init:get_argument(toolbar) of
        {ok, _} -> toolbar:start();
        _ -> false
    end.

args(VM, Global, bundles) ->
    case init:get_argument(bundles) of
        {ok, [Bundles]} ->
            lists:foreach(fun (Bundle) ->
                                  case catch beamjs_bundle:load(VM, Bundle) of
                                      {'EXIT', {{bundle, {throw, {error, #erlv8_object{} = E}}}, _}} ->
                                          io:format("Bundle error:~n~s~n", [E:proplist()]);
                                      _ -> ignore
                                  end
                          end,
                          Bundles);
        _ -> false
    end;

args(VM, Global, jseval) ->
    case init:get_argument(jseval) of
        {ok, [[JS]]} ->
            erlv8_vm:run(VM, erlv8_context:get(VM), JS, {"(command line)", 0, 0});
        _ -> false
    end;

args(VM, Global, path) ->
    case init:get_argument(jspath) of
        {ok, [Paths]} ->
            lists:foreach(fun (Path) ->
                                  Require = Global:get_value("require"),
                                  RPaths = Require:get_value("paths"),
                                  RPaths:unshift(Path)
                          end,
                          Paths);
        _ -> false
    end;

args(VM, Global, load) ->
    case init:get_argument(load) of
        {ok, [Files]} ->
            lists:foreach(fun(File) ->
                                  Global:set_value("xyz", "def222"),
                                  io:format("~s Changing XYZ ~s~n", [?MODULE, erlv8_vm:to_detail_string(VM, Global)]),
                                  Require = Global:get_value("require"),
                                  Global:set_value("module", ?V8Obj([])),
                                  Module = Global:get_value("module"),
                                  Module:set_value("id", File, [dontdelete, readonly]),
                                  Require:set_value("main", Module, [dontdelete, readonly]),
                                  case Require:call([File]) of
                                      {throw, {error, #erlv8_object{} = E}} -> io:format("~p~n", [E:proplist()]);
                                      _ -> ignore
                                  end
                          end,
                          Files);
        _ -> false
    end.

-define(REPL_START, "require('repl').start()").

main() ->
    %% [ t:t(X) || X <- [beamjs,erlv8_nif,{erlv8_vm,enqueue_tick}] ],
    %% [ t:t(X) || X <- [{beamjs_mod_require,require_fun},beamjs_mod_test] ],
    case os:getenv("ERLV8_SO_PATH") of
        false -> os:putenv("ERLV8_SO_PATH", "./deps/erlv8/priv")
    end,
    erlv8:start(),
    args(preemption),
    start(),
    {ok, VM} = erlv8_vm:start(),
    Global = erlv8_vm:global(VM),
    install_require(VM),
    Global:set_value("xyz", "def"),
    args(VM, Global, jseval),
    %% beamjs_bundle:load(VM, default),
    NoRepl = args(norepl),
    args(toolbar),
    args(VM, Global, path),
    args(VM, Global, bundles),
    args(VM, Global, load),
    io:format("~s Final: ~p~n", [?MODULE, Global:get_value("xyz")]),
    case NoRepl of
        true -> ok;
        false ->
            Global = erlv8_vm:global(VM),
            Global:set_value("module", ?V8Obj([])),
            Module = Global:get_value("module"),
            Module:set_value("id", "repl", [readonly, dontdelete]),
            Require = Global:get_value("require"),
            RequireObject = Require:object(),
            RequireObject:set_value("main", Module, [readonly, dontdelete]),
            erlv8_vm:run(VM, erlv8_context:get(VM), ?REPL_START, {"main", 0, 0}),
            receive _ -> ok end
    end,
    erlang:halt().

-include_lib("eunit/include/eunit.hrl").%

-ifdef(TEST).

-endif.
