
-module(connection_sup).
-author("pigbrain").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([create_connection/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = simple_one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = transient,
	Shutdown = 6000,

	Connection = {'connection', {connection, start_link, []},
		Restart, Shutdown, worker, [connection]},

	{ok, {SupFlags, [Connection]}}.

create_connection(Socket) ->
	supervisor:start_child(?MODULE, [Socket]).