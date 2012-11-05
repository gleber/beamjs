-module(beamjs_bundle).

%% -export([from_app_env/0,load/2, unload/2, modules/1, modules/2,bundles/0, add_bundle/1, remove_bundle/1, set_bundles/1]).
-export([load/2]).

-include_lib("erlv8/include/erlv8.hrl").

load(VM, Bundle) when is_atom(Bundle) -> load(VM, atom_to_list(Bundle));
load(VM, Bundle) when is_list(Bundle) ->
    Global = erlv8_vm:global(VM),
    Require = Global:get_value("require"),
    Globals = Require:call([filename:join(["lib_bundles", Bundle, "__globals__"])]),
    case Globals of
        #erlv8_object{} ->
            Globals:copy_properties_to(Global);
        {throw, {error, E}} ->
            error({bundle, {error, E:proplist()}});
        _V ->
            error({bundle, Globals})
    end,
    Modules = Require:call([filename:join(["lib_bundles", Bundle, "__modules__"])]),
    case Modules of
        #erlv8_object{} ->
            lists:foreach(fun({K, V}) ->
                                  erlv8_vm:stor(VM, {?MODULE, module, K}, V)
                          end,
                          Modules:proplist());
        {throw, {error, E2}} ->
            error({bundle, {error, E2:proplist()}});
        {throw, _} ->
            error({bundle, Modules})
    end,
    Bundle.
