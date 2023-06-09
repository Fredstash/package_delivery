%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are a solution that you can compare against yours to see
%% other options and to come up with even better solutions.
-module(update_vehicle_location_server).
-behaviour(gen_server).

%% API
-export([start_link/1,stop/0, update_vehicle_location/3]).

% %% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

% -record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
io:format("HELLO"),
    gen_server:start_link({local, Name}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

update_vehicle_location(Vehicle_Id, Lat, Lon) -> gen_server:cast(?MODULE, {Vehicle_Id, Lat, Lon}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	riakc_pb_socket:start_link("ryancoxerlangclass.com", 8087).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. The Request parameter is a tuple
%% consisting of a command, a binary that is the name of the person 
%% for which friends are to be stored, and a binary that is a list 
%% of friends. Both of these binaries can be created using term_to_binary.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(some_atom, _From, []) ->
    ok;
handle_call(_, _From, []) ->
	{stop,normal,
                server_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({Vehicle_id, Lat, Lon}, _Riak_PID) when not is_list(Vehicle_id) ->
    throw({badarg, {Vehicle_id, Lat, Lon}});

handle_cast({Vehicle_id, Lat, Lon}, _Riak_PID) when not is_number(Lat) ->
    throw({badarg, {Vehicle_id, Lat, Lon}});

handle_cast({Vehicle_id, Lat, Lon}, _Riak_PID) when not is_number(Lon) ->
    throw({badarg, {Vehicle_id, Lat, Lon}});

handle_cast({Vehicle_id, Lat, Lon}, Riak_PID) ->
    riak_api:put_vehicle_location(Vehicle_id, Lat, Lon, Riak_PID),
    {noreply, Riak_PID}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Eunit Tests
%%%===================================================================
-ifdef(EUNIT).
    -include_lib("eunit/include/eunit.hrl").
handle_update_test_()->
    {setup,
		fun()-> 
			meck:new(riak_api),
			meck:expect(riak_api, get_package, fun(_Package_id, _Riak_PID) -> {vehicle, history} end),
			meck:expect(riak_api, get_vehicle, fun(_Vehicle_id, _Riak_PID) -> {lat, lon} end),
			meck:expect(riak_api, put_vehicle_location, fun(_Vehicle_id, _Lat, _Lon, _Riak_PID) -> ok end)
		end,
		fun(_)-> 
			meck:unload(riak_api)
		end,
    [
        ?_assertEqual({noreply, [riakpid]},
        update_vehicle_location_server:handle_cast(
        {"123",35.0110, 115.4734}, [riakpid])),

        ?_assertThrow({badarg, {"123",35.0110, dogfarmer}},
        update_vehicle_location_server:handle_cast(
        {"123",35.0110, dogfarmer}, [riakpid])), %% Error Path

        ?_assertThrow({badarg, {"123",badatom, 115.4734}},
        update_vehicle_location_server:handle_cast(
        {"123",badatom, 115.4734}, [riakpid])), %% Error Path

        ?_assertThrow({badarg, {oopsallberries, 35.0110, 115.4734}},
        update_vehicle_location_server:handle_cast(
        {oopsallberries,35.0110, 115.4734}, [riakpid])) %% Error Path


    ]}.
-endif.
