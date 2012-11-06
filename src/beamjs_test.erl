-module(beamjs_test).

-include_lib("eunit/include/eunit.hrl").

%% -ifdef(TEST).

commonjs_test() ->
    t:t(erlv8_object),
    %% t:t(erlv8_fun),
    erlv8:start(),
    beamjs:start(),
    {ok, VM} = erlv8_vm:start(),
    Global = erlv8_vm:global(VM),
    beamjs:install_require(VM),
    Req = Global:get_value(<<"require">>),
    true = (Req /= undefined),
    [ beamjs_bundle:load(VM, X) || X <- [default, node_compat, commonjs] ],
    beamjs:load_main(VM, Global, "deps/commonjs/tests/unit-testing/1.0/program.js"),
    erlv8_vm:stop(VM).

%% -endif.
