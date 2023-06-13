-module(riak_api).
-export([get_package/2, get_vehicle/2]).
-export([put_vehicle_location/4]).


get_vehicle(_Vehicle_id, _Riak_PID) ->
	ok.

get_package(_Package_id, _Riak_PID) ->
	ok.

put_vehicle_location(_Vehicle_id, _Lat, _Lon, _Riak_PID) ->
	ok.