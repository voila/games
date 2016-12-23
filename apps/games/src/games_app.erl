-module(games_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	application:start(gproc),
	{ok, Cwd} = file:get_cwd(),
	PrivDir = Cwd ++ "/priv",
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", ws_handler, []},
			{"/:room", toppage_handler, []},
			%{"/", cowboy_static, {priv_file, games, "index.html"}},
			%{"/", cowboy_static, {file, PrivDir ++ "/index.html"}},
			%{"/static/[...]", cowboy_static, {priv_dir, games, "static"}}
			{"/priv/[...]", cowboy_static, {dir, PrivDir}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	games_sup:start_link().

stop(_State) ->
	ok.
