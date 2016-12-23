-module(games_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1, add_game/2]).

%% API.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_game(binary(),[{binary(), pid()}]) -> 'ok'.
add_game(Id, Players) ->
	GameID = list_to_atom(binary_to_list(Id)),
	error_logger:info_msg(">>> add_game(~p,~p)",[GameID, Players]),
	supervisor:start_child(?MODULE, 
	{GameID, 
		{games_game_srv, start_link, [GameID, Players]},
		transient, 1000, worker, [games_game_srv]}).


%% supervisor.
init([]) ->
	%% error_logger:info_msg(">>> rooms ~p",[application:get_env(rooms)]),

	{ok, Rooms} = application:get_env(rooms),
	Procs = lists:map(
		fun(R) -> 
			Room = list_to_atom(binary_to_list(R)), %% needs to be an atom
			{Room, 
         	{games_room_srv, start_link, [Room]},
         	permanent, 1000, worker, [games_room_srv]}
        end,
		Rooms
	),
	{ok, {{one_for_one, 10, 10}, Procs}}.
