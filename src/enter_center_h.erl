-module(enter_center_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Decoded_data = jsx:decode(Data),

    	Package_Id = binary_to_list(maps:get(<<"package_id">>, Decoded_data)),
    	Location_Id = binary_to_list(maps:get(<<"center_id">>, Decoded_data)),
    	Time = maps:get(<<"timestamp">>, Decoded_data),
	io:format("~p~n", [{Package_Id, Location_Id, Time}]),

    Return = update_location_server:update_location(arrived, Package_Id, Location_Id, Time),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, atom_to_binary(Return), Req0),
	{ok, Req, Opts}.
