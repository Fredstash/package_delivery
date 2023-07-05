-module(request_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	Decoded_data = jsx:decode(Data),

	%Package_Id = binary_to_list(maps:get(<<"package_id">>, Decoded_data)),
	Package_Id = maps:get(<<"package_id">>, Decoded_data),

	Location_History = request_package_server:request_location(Package_Id),
	io:format("~p~n", [Location_History]),
	Return = jsx:encode(term_to_binary(Location_History)),
	io:format("~p~n", [Return]),
	%io:format("~p~n", [jsx:decode(Return)]),
	%io:format("~p~n", [jsx:decode(binary_to_term(Return))]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Return, Req0),
	{ok, Req, Opts}.
