%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(anaMon_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

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
init([CacheLimit,WokerPoolSize]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    EventManChild = #{id => 'eventManager',
		      start => {events, start_mgr, [CacheLimit]},
		      restart => permanent,
		      shutdown => 5000,
		      type => worker,
		      modules => [events]},
    BatcherChild = #{id => 'Batcher',
		      start => {batcher, start_link, []},
		      restart => permanent,
		      shutdown => 5000,
		      type => worker,
		      modules => [batcher]},
    WorkerPoolManChild = #{id => 'workerPoolMan',
		      start => {worker_pool_man, start_link, 
				[{init_tab_size,WokerPoolSize}]},
		      restart => permanent,
		      shutdown => 5000,
		      type => worker,
		      modules => [worker_pool_man]},

    {ok, {SupFlags, [EventManChild,BatcherChild,WorkerPoolManChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
