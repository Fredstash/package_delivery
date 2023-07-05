%%%-------------------------------------------------------------------
%% @doc package_delivery public API
%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc db_access public API
%% @end
%%%-------------------------------------------------------------------

-module(package_delivery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
        % Test case
	    {"/", toppage_h, []},
        % update location
		{"/register_package", register_package_h,[]},
		{"/enter_center", enter_center_h,[]},
		{"/put_on_vehicle", put_on_vehicle_h,[]},
		{"/mark_delivered", mark_delivered_h,[]},
        % Update location
		{"/update_vehicle_location", update_vehicle_location_h,[]},
        % Request location
		{"/request_location", request_package_h,[]}

	    ]}
	]),

	PrivDir = code:priv_dir(package_delivery),
	io:format("~p~n", [PrivDir]),
        {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
	package_delivery_sup:start_link().
stop(_State) ->
    ok.

%% internal functions


%% internal functions
