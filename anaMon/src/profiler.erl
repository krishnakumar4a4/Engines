%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(profiler).

%% API
-export([combination/2,print_stats/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

combination(Unique,Repeat) ->
	[erlang:spawn_link(fun() -> [erlang:send(whereis(events),{like_url,Name})||_N<-lists:seq(1,Repeat)] end)||Name<-lists:seq(1,Unique)].

print_stats() ->
    [io:format("Message que len of Pid:~p is ~p~n",
	       [Pid,element(2,erlang:process_info(Pid,message_queue_len))])
     ||{_,Pid,_,_}<-supervisor:which_children(worker_pool_sup)],
    io:format("Worker Pool count ~p~n",[supervisor:count_children(worker_pool_sup)]).
    
