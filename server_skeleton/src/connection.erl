
-module(connection).
-author("pigbrain").

-behaviour(gen_fsm).

-export([start_link/1]).

-export([init/1,
	state_name/2,
	state_name/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

-export([connected/2]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

start_link(Socket) ->
	gen_fsm:start_link(?MODULE, Socket, []).

init(Socket) ->
	error_logger:info_report([{new_connection, {socket, Socket}}]),
	{ok, connected, #state{socket = Socket}}.

state_name(_Event, State) ->
	{next_state, state_name, State}.

state_name(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

handle_info({tcp_closed, _Socket}, StateName, StateData) ->
	io:format("tcp_closed event ~p, ~p, ~n", [StateName, StateData]),
	{stop, normal, StateData};

handle_info({tcp, Socket, Data}, StateName, #state{socket=Socket} = State) ->
	error_logger:info_report([{tcp, Socket, Data, StateName}]),
	gen_tcp:send(Socket, <<"hello yaya">>),
	{next_state, StateName, State};

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


connected(Event, State) ->
	error_logger:info_report([{connected_event, {Event, State}}]).