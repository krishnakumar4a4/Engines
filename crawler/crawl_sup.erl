%%%-------------------------------------------------------------------
%%% @author krishnak
%%% @copyright (C) 2017, krishnak
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2017 by krishnak
%%%-------------------------------------------------------------------
-module(crawl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    HttpdChild = #{id => 'httpd_crawl',
	       start => {'httpd_crawl', start_link, [9000]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => ['httpd_crawl']},
    CrawlChild = #{id => 'crawl',
	       start => {'crawl', start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => ['crawl']},
    HttpdTestChild = #{id => 'httpd_test',
	       start => {'httpd_test', start_link, [9001]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => ['httpd_test']},

    {ok, {SupFlags, [CrawlChild,HttpdTestChild,HttpdChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
