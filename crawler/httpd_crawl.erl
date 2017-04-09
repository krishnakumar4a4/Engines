%%%-------------------------------------------------------------------
%%% @author krishnak
%%% @copyright (C) 2017, krishnak
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2017 by krishnak
%%%-------------------------------------------------------------------
-module(httpd_crawl).
-export([start/1,start_link/1,handle_request/1]).

start_link(Port) ->
    link(start(Port)).

start(Port)->
    io:format("Started listeners~n"),
    {ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
    loop(ListenSock).

loop(ListenSock) ->	
    %%io:format("Started accepts~n"),
    {ok, Sock}=gen_tcp:accept(ListenSock),
    spawn(?MODULE, handle_request, [Sock]),
    loop(ListenSock).


handle_request(Sock) ->
    {ok, {http_request, Method, _Path, _Version}}=gen_tcp:recv(Sock, 0),
    case (Method) of
	'POST' ->
	    %%io:format("handling post~n"),
	    handle_post(Sock);
	_ ->
	    %%io:format("handle unsupported~n"),
	    send_unsupported_error(Sock)
    end.

get_content_length(Sock) ->
    case gen_tcp:recv(Sock, 0, 60000) of
	{ok, {http_header, _, 'Content-Length', _, Length}} -> 
	    list_to_integer(Length);
	{ok, {http_header, _, _Header, _, _}} -> 
	    get_content_length(Sock)
    end.

get_body(Sock, Length) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, http_eoh} ->
	    inet:setopts(Sock, [{packet, raw}]),
	    {ok,Body}=gen_tcp:recv(Sock, Length),Body;
	_ -> 
	    get_body(Sock, Length)
    end.

handle_post(Sock) ->
    Length=get_content_length(Sock),
    PostBody=get_body(Sock, Length),
    %%io:format("Handling Post data ~p~n",[PostBody]),
    case gen_server:call(crawl,{post,PostBody},infinity) of
	ok ->
	    send_accept(Sock);
	Reason ->
	    send_error(Sock,Reason)
    end.

send_accept(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
    gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).

send_error(Sock,Reason) ->
    gen_tcp:send(Sock, "HTTP/1.1 405 "++tuple_to_list(Reason)++"\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).
