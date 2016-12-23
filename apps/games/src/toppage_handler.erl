
%% Feel free to use, reuse and abuse the code in this file.

-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Room, _} = cowboy_req:binding(room, Req), 
	error_logger:info_msg(">>> binding ~p",[Room]),
	{Status, Body} = get_response(Room),
	{ok, Req2} = cowboy_req:reply(Status,
		[{<<"content-type">>, <<"text/html">>}],
		Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_response(Room) ->
	error_logger:info_msg(">>> get_resp ~p - ~p",[Room, application:get_env(games, rooms)]),
	{ok,Rooms} = application:get_env(games, rooms),
	case lists:member(Room, Rooms) of
		true -> 
			Binary = get_html(Room),
			{200, Binary};
		false -> {404, <<>>}
	end.

get_html(Room) ->
	{ok, Cwd} = file:get_cwd(),
	Folder = "priv/" ++ binary_to_list(Room),
    error_logger:info_msg("~p ~p", [Room, Folder]),
	Filename = filename:join([Cwd, Folder, "index.html"]),
    error_logger:info_msg("~p", [Filename]),
    {ok, Binary} = file:read_file(Filename),
	Binary.