%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(worker_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add/1,count/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => lil_worker,
	       start => {worker, start_link, []},
	       restart => permanent,
	       shutdown => 5000
%%	       type => worker,
%%	       modules => ['AModule']
	      },

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add(Count) when is_integer(Count) ->
    [supervisor:start_child(worker_pool_sup,[])||_N<-lists:seq(1,Count)].

count() ->
    supervisor:count_children().

