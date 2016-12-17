-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(st, {name, order, game, game_id, waiting=[]}).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #st{}}.

websocket_handle({text, Json}, Req, State) ->
  Msg = jsx:decode(Json, [return_maps]),
  handle_msg(Msg, State),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
  error_logger:info_msg("Pid:~p {msg, ~p}",[self(),Msg]),
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({join, Game, Name}, Req, State) ->
  gproc:reg({p,l,Game}),
  {ok, Req, State#st{name=Name, game=Game}};

websocket_info({start, GameId, Order}, Req, #st{name=Name,game=Game}=State) ->
  gproc:unreg({p,l,Game}), %% leave the waiting room
  gproc:reg({p,l, GameId}), %% join a started game
  %%error_logger:info_msg("~p has started a game~n",[Name]),
  O = list_to_binary(integer_to_list(Order)),
  Msg = [{<<"key">>, <<"started">>}, {<<"val">>, O}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State#st{game_id=GameId, order=Order}};

websocket_info({move, Move, Order}, Req, State) ->
  O = list_to_binary(integer_to_list(Order)),
  Val = << Move/binary, "@", O/binary >>,  
  Msg = [{<<"key">>, <<"move">>},{<<"val">>, Val}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, State};

websocket_info({quit, GameId, Name}, Req, #st{game=Game}=State) ->
  gproc:unreg({p,l,GameId}), %% leave the started game
  %% gproc:reg({p,l, Game}), %% join the waiting room again 
  Msg = [{<<"key">>, <<"quit">>},{<<"val">>, Name}],
  Json = jsx:encode(Msg),
  {reply, {text, Json}, Req, #st{game=Game}};

websocket_info({name, From}, Req, #st{name=Name}=State) ->
  From ! {msg, [{<<"key">>, <<"joined">>}, {<<"val">>, Name}]},
  {ok, Req, State}; 

websocket_info(_Info, Req, State) ->
   {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_msg(
  #{<<"msg">> := <<"players">>, <<"game">> := Game}, State) -> 
  Pids = gproc:lookup_pids({p, l, Game}),
  %self() ! {msg, [{<<"key">>, <<"players">>}, {<<"val">>, Players}]};
  [P ! {name, self()} || P <- Pids];

handle_msg(
  #{<<"msg">> := <<"join">>, <<"game">> := Game, <<"player">> := Name}, State) -> 
  gproc:reg({n,l, Name}), %% register own name (must be unique)
  gproc:send({n,l, Name}, {join, Game, Name}), 
  error_logger:info_msg("~p has joined~n",[Name]),
  gproc:send({p,l, Game}, {msg, [{<<"key">>, <<"joined">>}, {<<"val">>, Name}]});
  % [{<<"key">>, <<"join">>}, {<<"val">>, true}];

handle_msg(
  #{<<"msg">> := <<"start">>, <<"players">> := Names}, #st{name=Name}=State) -> 
  %% TODO need to check whether Names are in the waiting room
  
  GameId = list_to_binary(uuid:to_string(uuid:uuid4())),
  Names2 = [Name|Names],
  [gproc:send({n,l,N}, {start, GameId, O}) || 
  {N, O} <- lists:zip(Names2, lists:seq(1, length(Names2)))];
  %[{<<"key">>, <<"start">>}, {<<"val">>, true}];

handle_msg(
  #{<<"msg">> := <<"move">>, <<"move">> := Move}, 
  #st{name=Name, game_id=GameId, order=Order}=State) -> 
  gproc:send({p,l,GameId}, {move, Move, Order});
  %[{<<"key">>, <<"moved">>}, {<<"val">>, true}];

handle_msg(
  #{<<"msg">> := <<"quit">>}, #st{name=Name, game_id=GameId}=State) -> 
  gproc:send({p, l, GameId}, {quit, GameId, Name});
  %[{<<"key">>, <<"quited">>}, {<<"val">>, true}];

handle_msg(_, _) -> ok.

