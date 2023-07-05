-module(riak_api).
-export([get_package/2, get_vehicle/2]).
-export([put_vehicle_location/4]).
%-export([update_vehicle/3, update_history/3]).
-export([update_package/3]).
-export([put_package/3]).
-export([delete_package/2]).


get_vehicle(Vehicle_id, Riak_PID) ->
	case riakc_pb_socket:get(Riak_PID, <<"vehicle">>, term_to_binary(Vehicle_id)) of
		{ok, Fetched} ->
			binary_to_term(riakc_obj:get_value(Fetched));
		Error ->
			io:format("Error: ~p~n", [Error]),
		       	Error
	end.

get_package(Package_id, Riak_PID) ->
	case riakc_pb_socket:get(Riak_PID, <<"package">>, term_to_binary(Package_id)) of
		{ok, Fetched} ->
			binary_to_term(riakc_obj:get_value(Fetched));
		Error -> 
			io:format("~p~n",[Package_id]),
			Error
	end.

update_package(Package_id, Data={Location_id, Time, Cmd}, Riak_PID) ->
	case riakc_pb_socket:get(Riak_PID, <<"package">>, term_to_binary(Package_id)) of
		{ok, Fetched} ->
			[_|[Current_History]] = binary_to_term(riakc_obj:get_value(Fetched)),
			%Request = riakc_obj:new(<<"package">>, term_to_binary(Package_id), term_to_binary([Location_id,[Data|Current_History]])),
			%riakc_pb_socket:put(Riak_PID, Request);
			put_package(Package_id, [Data|Current_History], Riak_PID);
		{error, inotfound} ->
			io:format("not found~p~n",[Data]),
			%Request = riakc_obj:new(<<"package">>, term_to_binary(Package_id), term_to_binary([Location_id,[Data]])),
			%PutReturn = riakc_pb_socket:put(Riak_PID, Request),
			put_package(Package_id, [Data], Riak_PID);
		Error ->
			io:format("Error: ~p~n", [Error]),
			Error
		end.

put_package(Package_id, Data=[{Location_id,_,_}|_], Riak_PID) ->
	Request = riakc_obj:new(<<"package">>, term_to_binary(Package_id), term_to_binary([Location_id,Data])),
	riakc_pb_socket:put(Riak_PID, Request).


put_vehicle_location(Vehicle_id, Lat, Lon, Riak_PID) ->
	 Request = riakc_obj:new(<<"vehicle">>, term_to_binary(Vehicle_id), term_to_binary({Lat, Lon})),
	 PutReturn = riakc_pb_socket:put(Riak_PID, Request),
	 io:format("~p~n",[PutReturn]),
         PutReturn.


% For testing purposes.
delete_package(Package_id, Riak_PID) ->
	riakc_pb_socket:delete(Riak_PID, <<"package">>, term_to_binary(Package_id)).
