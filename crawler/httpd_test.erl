%%%-------------------------------------------------------------------
%%% @author krishnak
%%% @copyright (C) 2017, krishnak
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2017 by krishnak
%%%-------------------------------------------------------------------
-module(httpd_test).
-export([start_link/1]).

start_link(Port) ->
    inets:stop(),
    inets:start(),
    inets:start(httpd, [{port, Port},{server_name,"httpd_test"},
			{server_root,"server_root"},
			{document_root,"doc_root"},
			{bind_address, "localhost"}]).
