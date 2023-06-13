-module(update_vehicle_location_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Decoded_data = jsx:decode(Data),
    [Vehicle_Id|[Lat|[Lon]]] = Decoded_data,
    update_vehicle_location_server:update_vehicle_location(Vehicle_Id, Lat, Lon),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	% Req = cowboy_req:reply(200, #{
	% 	<<"content-type">> => <<"text/json">>
	% }, Return, Req0),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, [], Req0),
	{ok, Req, Opts}.