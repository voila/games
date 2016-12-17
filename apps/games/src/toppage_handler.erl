
%% Feel free to use, reuse and abuse the code in this file.

-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    Game = cowboy_req:binding(game, Req), 
	Html = get_html(Game),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html(Game) ->
	{ok, Cwd} = file:get_cwd(),
    Folder = "priv/" ++ binary_to_list(<<"connect4">>),
    error_logger:info_msg("~p ~p", [Game,Folder]),
	Filename = filename:join([Cwd, Folder, "index.html"]),
        error_logger:info_msg("~p", [Filename]),
    {ok, Binary} = file:read_file(Filename),
	Binary.