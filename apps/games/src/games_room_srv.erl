-module(games_room_srv).

-behaviour(gen_server).

%% API
-export([start_link/1,
        join/2,
        players/1,
        start/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    players :: [{binary(),pid()}]
    }).

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
start_link(Room) ->
    gen_server:start_link({local, Room}, ?MODULE, [], []).

-spec join(atom(), binary()) -> {atom(), binary()} .
join(Room, Name) ->
    gen_server:call(Room, {join, Name}).

-spec players(atom()) -> [binary()].
players(Room) ->
    gen_server:call(Room, players).

-spec start(atom(), binary(), [binary()]) -> {atom(), binary()}.
start(Room, Name, Names) ->
    gen_server:call(Room, {start, Name, Names}).
    
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
init([]) ->
    %% error_logger:info_msg(">>> ~p",[Room]),
    {ok, #state{players=[]}}.

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
handle_call(players, _From, #state{players=Players}=State) ->
    Names = [N || {N, _} <- Players], 
    {reply, Names, State};

handle_call({join, Name}, {Pid, _Ref}, #state{players=Players}=State) ->
    Players2 = [{Name, Pid}|Players], 
    %% check that Name is unique
    Reply = case proplists:lookup(Name, Players) of
        none -> 
            %% broadcast new new name to all players
            bc({joined, Name}, Players),
            {ok, <<>>}; 
        _Else -> {error, <<Name/binary, " already exists!">>}
    end,
    {reply, Reply, State#state{players=Players2}};

handle_call({start, Name, Names}, _From, #state{players=Players}=State) ->
    GameId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    {Reply, Players2} = 
    case lists:all(fun(N) -> name_uniq(N, Players) end, Names) of
        false -> {{error, <<"Opponent unavailable!">>}, Players};
        true -> 
            InGame = [P || P <- [proplists:lookup(N, Players) || N <- [Name|Names]], P =/= none], 
            games_sup:add_game(GameId, InGame),
            InRoom = [proplists:delete(N, Players) || N <- [Name|Names]],
            bc({start, GameId}, InGame),
            {{ok, GameId}, InRoom}
    end,
    {reply, Reply, State#state{players=Players2}};

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
bc({joined, Name}, Players) ->
    error_logger:info_msg("bc {joined, ~p}", [Name]),
    [P ! {join, Name} || {_, P} <- Players];
bc({start, GameId}, Players) ->
    [P !{start, GameId, O} || {{_,P}, O} <- lists:zip(Players, lists:seq(1, length(Players)))].

name_uniq(N, Players) -> %% TODO name this function
    case proplists:lookup(N, Players) of
        none -> false;
        _Found -> true
    end.
