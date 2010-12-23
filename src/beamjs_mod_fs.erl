-module(beamjs_mod_fs).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"readFile", fun read_file/2}]).

read_file(#erlv8_fun_invocation{},[Filename, Encoding, #erlv8_fun{} = Callback]) when is_list(Filename) andalso is_list(Encoding)  ->
	case file:read_file(Filename) of
		{ok, B} when is_binary(B) ->
			Callback:call([null, binary_to_list(B)]);
		{error, noent} ->
			Callback:call([{error, "File not found"},""])
	end;

read_file(#erlv8_fun_invocation{}=I,[Filename, #erlv8_fun{} = Callback]) when is_list(Filename)  ->
	read_file(I,[Filename, "utf-8", Callback]);
read_file(#erlv8_fun_invocation{},[Filename, Encoding]) when is_list(Filename) andalso is_list(Encoding)  ->
	{throw, {error, "No callback specified"}}.


			
	
