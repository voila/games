-module(games_game_srv).

-behaviour(gen_server).

%% API
-export([start_link/2, move/3, quit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {game_id :: binary(), players :: [{binary(),pid()}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(GameId, Players) ->
    gen_server:start_link({local, GameId}, ?MODULE, [GameId, Players], []).

-spec move(atom(), binary(), binary()) -> 'ok'.
move(Id, Move, Order) ->
    error_logger:info_msg(">>> move(~p, ~p, ~p)", [Id, Move,Order]),
    %% move('cf227597-0a93-48d9-a283-92036385f006', <<"1,1">>, <<"1">>)
    gen_server:cast(Id, {move, Move, Order}).

-spec quit(atom(), binary()) -> 'ok'.
quit(Id, Name) ->
    gen_server:cast(Id, {quit, Name}).

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
init([GameId, Players]) ->
    error_logger:info_msg(">>> GameId ~p",[GameId]),
    error_logger:info_msg(">>> Players ~p",[Players]),
    {ok, #state{game_id=GameId, players=Players}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({move, Move, Order}, #state{players=Players}=State) ->
    error_logger:info_msg(">>> handle_cast {move, ~p, ~p} --> ~p", [Move,Order, Players]),
    bc({move, Move, Order}, Players),
    {noreply, State};

handle_cast({quit, Name}, #state{game_id=GameId, players=Players}=State) ->
    Name = 
    bc({quit, GameId, Name}, Players),
    {stop, quit, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

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
bc({move, Move, Order}, Players) ->
    error_logger:info_msg(">>> {move, ~p, ~p} --> ~p", [Move,Order, Players]),
    % Msg = [{<<"key">>, <<"joined">>}, {<<"val">>, Name}],
    [P ! {move, Move, Order} || {_,P} <- Players];

bc({quit, GameId, Name}, Players) ->
    {Name,_} = proplists:lookup(Name),
    [P ! {quit, GameId, Name} || {_,P} <- Players].