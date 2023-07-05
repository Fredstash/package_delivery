-module(register_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	%io:format("~p~n", [binary_to_list(Data)]),
	Decoded_data = jsx:decode(Data),
	%io:format("~p~n", [Decoded_data]),
    Location_Id = binary_to_list(maps:get(<<"center_id">>, Decoded_data)),
    Package_Id = binary_to_list(maps:get(<<"package_id">>, Decoded_data)),
    Time = maps:get(<<"timestamp">>, Decoded_data),

  update_location_server:update_location(arrived, Package_Id, Location_Id, Time),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, atom_to_binary(ok), Req0),
	{ok, Req, Opts}.
