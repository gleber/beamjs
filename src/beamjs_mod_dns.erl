-module(beamjs_mod_dns).

-export([exports/1, init/1]).

-behaviour(beamjs_module).

-include_lib("erlv8/include/erlv8.hrl").

init(_VM) -> ok.

exports(_VM) ->
    ?V8Obj([{"__doc__", "Node.js compatible DNS module."},
            {"resolve4",
             erlv8_fun:new(fun resolve4/2,
                           ?V8Obj([{"__doc__",
                                    "`resolve4(name, callback)`\n\nResolves IPv4 address "
                                    "for given name\n\nExample:\n\n    dns.resolve('beamjs.org', "
                                    "function(err, ips){\n        if (err) throw err;\n  "
                                    "      ips.forEach(function(ip) { console.log(ip) });\n "
                                    "   })"}]))},
            {"reverse",
             erlv8_fun:new(fun reverse/2,
                           ?V8Obj([{"__doc__",
                                    "`reverse(ip, callback)`\n\nReverse resolves an ip address "
                                    "to an array of domain names.\n\nExample:\n\n    dns.resolve('"
                                    "174.129.212.2', function(err, names){\n        if (err) "
                                    "throw err;\n        names.forEach(function(name) { console.lo"
                                    "g(name) });\n    })"}]))}]).

%%
%% Exposed Functions
%%

resolve4(#erlv8_fun_invocation{}, [Name, #erlv8_fun{} = Callback])
  when is_list(Name) ->
    case inet_res:resolve(Name, any, a) of
        {ok, Msg} ->
            {anlist, Records} = lists:keyfind(anlist, 1, inet_dns:msg(Msg)),
            Ips = [ip_from_record(V1) || V1 <- Records],
            Callback:call([null, ?V8Arr([ip4_to_string(V2) || V2 <- Ips])]);
        {error, _} -> Callback:call([{error, "Cannot resolve"}, Name])
    end.

reverse(#erlv8_fun_invocation{}, [Ip, #erlv8_fun{} = Callback])
  when is_list(Ip) ->
    case inet:gethostbyaddr(Ip) of
        {ok, Hostent} ->
            {hostent, Address, _, _, _, _} = Hostent,
            Callback:call([null, ?V8Arr([Address])]);
        {error, _} -> Callback:call([{error, "Cannot reverse resolve"}, Ip])
    end.

                                                %
                                                % Utility Functions
                                                %

ip_from_record(Record) ->
    {_, Ip} = lists:keyfind(data, 1, inet_dns:rr(Record)), Ip.

ip4_to_string(Ip) ->
    {One, Two, Three, Four} = Ip,
    lists:concat([One, '.', Two, '.', Three, '.', Four]).
