-module(beamjs_test).

-include_lib("erlv8/include/erlv8.hrl").

-include_lib("eunit/include/eunit.hrl").

%% -ifdef(TEST).

commonjs_test() ->
    %% t:t(erlv8_object),
    %% t:t(erlv8_fun),
    erlv8:start(),
    beamjs:start(),
    {ok, VM} = erlv8_vm:start(),
    Global = erlv8_vm:global(VM),
    beamjs:install_require(VM),
    Req = Global:get_value(<<"require">>),
    Root = case file:get_cwd() of %% figure out root of the project even if we are in .eunit
               {ok, Path} ->
                   case lists:reverse(filename:split(Path)) of
                       [".eunit" | Rest] -> filename:join(lists:reverse(Rest));
                       _ -> Path
                   end;
               _ -> "."
           end,
    Req:set_value("paths", ?V8Arr([Root])),
    %% io:format(user, "initial paths: ~p~n", [(Req:get_value("paths")):list()]),
    %% io:format(user, "initial cwd: ~p~n", [file:get_cwd()]),
    true = (Req /= undefined),
    [ beamjs_bundle:load(VM, X) || X <- [default, node_compat, commonjs] ],
    beamjs:load_main(VM, Global, "deps/commonjs/tests/unit-testing/1.0/program.js"),
    erlv8_vm:stop(VM).

%% -endif.
