
-module(server_app).
-author("pigbrain").

-behaviour(application).

-export([start/2,
	stop/1]).


start(_StartType, _StartArgs) ->
	case server_sup:start_link() of
		{ok, Pid} ->
			io:format("server started!~n"),
			{ok, Pid};
		Error ->
			io:format("server cant not start. ~p~n", Error),
			Error
	end.

stop(_State) ->
	ok.
