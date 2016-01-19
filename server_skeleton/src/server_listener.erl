
-module(server_listener).
-author("pigbrain").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([acceptor/1]).

-define(SERVER, ?MODULE).
-define(LISTEN_PROCESS_COUNT, 10).
-define(PORT, 9000).
-define(TCP_OPTIONS, [binary, {nodelay, true}, {packet, 0}, {active, true}, {reuseaddr, true}]).

-record(state, {
	listen_socket,
	connection_count = 0
}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	case gen_tcp:listen(?PORT, ?TCP_OPTIONS) of
		{ok, ListenSocket} ->
			spawn_acceptor_pool(ListenSocket),
			{ok, #state{listen_socket = ListenSocket}};
		Reason ->
			{stop, Reason}
	end.


handle_call({connect, ClientSocket},
		_From,
		#state{connection_count = ConnectionCount} = State) ->

	Result = connection_sup:create_connection(ClientSocket),
	{reply, Result, State#state{connection_count = ConnectionCount + 1}};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


spawn_acceptor_pool(ListenSocket) ->
	spawn_acceptor_pool(ListenSocket, ?LISTEN_PROCESS_COUNT).

spawn_acceptor_pool(_ListenSocket, 0) ->
	done;

spawn_acceptor_pool(ListenSocket, ProcessCount) ->
	spawn_link(?MODULE, acceptor, [ListenSocket]),
	spawn_acceptor_pool(ListenSocket, ProcessCount - 1).

acceptor(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			case gen_server:call(?MODULE, {connect, AcceptSocket}) of
				{ok, Pid} ->
					gen_tcp:controlling_process(AcceptSocket, Pid),
					acceptor(ListenSocket);
				Error ->
					error_logger:info_report([{acceptor, Error}]),
					acceptor(ListenSocket)
			end;
		Error ->
			error_logger:info_report([{acceptor, Error}]),
			acceptor(ListenSocket)
	end.
