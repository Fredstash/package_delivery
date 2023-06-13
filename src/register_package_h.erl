-module(register_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Decoded_data = jsx:decode(Data),
    [Package_Id|[Location_Id|[Time]]] = Decoded_data,
    Return = update_location_server:update_location(arrived, Package_Id, Location_Id, Time),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Return, Req0),
	{ok, Req, Opts}.