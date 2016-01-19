-module(server_sup).
-author("pigbrain").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,

	ServerListener = {server_listener, {server_listener, start_link, []},
			Restart, Shutdown, worker, [server_listener]},

	ConnectionSupervisor = {connection_sup, {connection_sup, start_link, []},
			Restart, Shutdown, supervisor, [connection_sup]},

	{ok, {SupFlags, [ServerListener, ConnectionSupervisor]}}.
