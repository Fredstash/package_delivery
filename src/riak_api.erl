-module(riak_api).
-export([get_package/2, get_vehicle/2]).
-export([put_package/3, put_vehicle_location/4]).
-export([update_history/3]).


get_vehicle(_Vehicle_id, _Riak_PID) ->
	ok.

get_package(Package_id, Riak_PID) ->
	case riakc_pb_socket:get(Riak_PID, <<"package">>, Package_id) of
		{ok, Fetched} ->
			{reply, binary_to_term(riakc_obj:get_value(Fetched)), Riak_PID};
		Error -> 
			{reply, Error, Riak_PID}
	end.
	% ok.

update_history(_Package_id, _Append_History, _Riak_PID) ->

	ok.

put_package(Package_id, Data, Riak_PID) ->
	Request=riakc_obj:new(<<"package">>, Package_id, Data),
	{reply, riakc_pb_socket:put(Riak_PID, Request), Riak_PID}.
	% ok.

put_vehicle_location(_Vehicle_id, _Lat, _Lon, _Riak_PID) ->
	ok.