-module(request_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Decoded_data = jsx:decode(Data),
	% Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
    [_Cmd|[Package_Id]] = Decoded_data,
    % Return = request_package_server:request_location(Cmd, Package_Id),
    Return = request_package_server:request_location(request_location, Package_Id),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Return, Req0),
	{ok, Req, Opts}.
