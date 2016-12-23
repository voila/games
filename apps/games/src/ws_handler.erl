-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(st, {}).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #st{}}.

websocket_handle({text, Json}, Req, State) ->
  error_logger:info_msg("JSON ~p",[Json]),
  Msg = jsx:decode(Json, [return_maps]),
  handle_msg(Msg),
  {ok, Req, State};
  % Resp = handle_msg(Msg, State),
  % Json = jsx:encode(Resp),
  % {ok, {text, Json}, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
  error_logger:info_msg("Pid:~p {msg, ~p}",[self(),Msg]),
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({join, Name}, Req, State) ->
  %%gproc:reg({p,l,Game}),
  Msg = [{<<"key">>, <<"joined">>}, {<<"val">>, Name}],
  error_logger:info_msg(">>> websocket_info({join, Name} ~p ",[Msg]),
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({start, GameId, Order}, Req, State) ->
  %gproc:unreg({p,l,Game}), %% leave the waiting room
  %gproc:reg({p,l, GameId}), %% join a started game
  error_logger:info_msg(">>> ~p ",[{start, GameId, Order}]),
  O = list_to_binary(integer_to_list(Order)),
  Msg = [{<<"key">>, <<"started">>}, {<<"val">>, << GameId/binary, ",", O/binary >>}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({move, Move, Order}, Req, State) ->
  Val = << Move/binary, "@", Order/binary >>,  
  Msg = [{<<"key">>, <<"move">>},{<<"val">>, Val}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({quit, GameId, Name}, Req, State) ->
  gproc:unreg({p,l,GameId}), %% leave the started game
  %% gproc:reg({p,l, Game}), %% join the waiting room again 
  Msg = [{<<"key">>, <<"quit">>},{<<"val">>, Name}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

% websocket_info({players, Players}, Req, #st{name=Name}=State) ->
%   From ! {msg, [{<<"key">>, <<"joined">>}, {<<"val">>, Name}]},
%   {ok, Req, State}; 

websocket_info(_Info, Req, State) ->
   {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_msg(
  #{<<"msg">> := <<"players">>, <<"game">> := Game}) -> 
  % Pids = gproc:lookup_pids({p, l, Game}),
  % [P ! {name, self()} || P <- Pids];
  Room = list_to_atom(binary_to_list(Game)),
  Names = games_room_srv:players(Room),
  error_logger:info_msg(">>> players? ~p",[Names]),
  [self() ! {msg, [{<<"key">>, <<"joined">>}, {<<"val">>, N}]} || N <- Names];

handle_msg(
  #{<<"msg">> := <<"join">>, <<"game">> := Game, <<"player">> := Name}) -> 
  % gproc:reg({n,l, Name}), %% register own name (must be unique)
  % gproc:send({n,l, Name}, {join, Game, Name}), 
  % error_logger:info_msg("~p has joined~n",[Name]),
  % gproc:send({p,l, Game}, {msg, [{<<"key">>, <<"joined">>}, {<<"val">>, Name}]});
  Room = list_to_atom(binary_to_list(Game)),
  Resp = games_room_srv:join(Room, Name),
  error_logger:info_msg(">>> join? ~p",[Resp]);

handle_msg(
  #{<<"msg">> := <<"start">>, <<"game">> := Game, <<"player">> := Name, <<"players">> := Names}) -> 
  % GameId = list_to_binary(uuid:to_string(uuid:uuid4())),
  % Names2 = [Name|Names],
  % [gproc:send({n,l,N}, {start, GameId, O}) || 
  % {N, O} <- lists:zip(Names2, lists:seq(1, length(Names2)))];
  Room = list_to_atom(binary_to_list(Game)),
  Resp = games_room_srv:start(Room, Name, Names),
  error_logger:info_msg(">>> start? ~p",[Resp]);


handle_msg(
  #{<<"msg">> := <<"move">>, <<"game_id">> := GameId, <<"move">> := Move, <<"order">> := Order}) -> 
  %%v {'msg':'move', 'game_id':'4e1ad91a-ec00-4d8d-a5e2-1a6610b83c3d','move':'1,1','order':'1'}"
  Id = list_to_atom(binary_to_list(GameId)),
  error_logger:info_msg(">>> gameId as atom ~p",[Id]),
  Resp = games_game_srv:move(Id, Move, Order),
  error_logger:info_msg(">>> move? ~p",[Resp]);
  % gproc:send({p,l,GameId}, {move, Move, Order}),
  % State;

handle_msg(
  #{<<"msg">> := <<"quit">>, <<"game_id">> := GameId, <<"player">> := Name}) -> 
  Id = list_to_atom(binary_to_list(GameId)),
  Resp = games_game_srv:quit(Id, Name),
  error_logger:info_msg(">>> quit? ~p",[Resp]);
  % gproc:send({p, l, GameId}, {quit, GameId, Name}),
  % State;


handle_msg(_Msg) -> ok.

