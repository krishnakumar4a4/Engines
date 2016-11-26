%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(batcher).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tables = [] %%List of ets tables
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #state{}}.

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
handle_cast({takeover,TabId},State) ->
    case State#state.tables of
	[] ->
	    %%Trigger an event to start loading up the data
	    erlang:spawn(fun() -> collect_data_from_workers(TabId) end),
	    {noreply,State#state{tables=[TabId]}};
	TableList ->
	    %%If there are multiple tables we can try combining them
	    %%as well, but it wouldn't be better than taking care of
	    %%individual tables as combining them leads to N^2 complexity
	    RevList = lists:reverse(TableList),
	    %%Trigger an event to start loading up the data
	    erlang:spawn(fun() -> collect_data_from_workers(TabId) end),
	    {noreply,State#state{tables=[TabId|RevList]}}
    end;
handle_cast({get_permission,Tid,Pid},State) ->
    %%Give control of accessing database to spawned process
    ets:give_away(Tid,Pid,[]),
    {noreply,State};
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

collect_data_from_workers(Tid) ->
    gen_server:cast(batcher,{get_permission,Tid,self()}),
    %%Lock the persistent database and start loading the data
    %%Here we are storing each and every table data in a file
    %%Files to be processed in batch to update the DB
    {Output,FreePids} = ets:foldl(fun({Url,Pid},{Out,PidList}) -> 
			       {[{Url,gen_server:call(Pid,get,100)}|Out],
				[Pid|PidList]} end,{[],[]},Tid),
    case file:open("tab"++integer_to_list(Tid),[write]) of
	{ok,IoDev} ->
	    io:format(IoDev,"~p~n",[Output]),
	    io:format("Data written to file~n"),
	    file:close(IoDev),
	    %%If successfully written to file,remove the table from list
	    gen_server:cast(batcher,{update_tables,Tid}),
	    %%Also add the free worker Pids to worker_pool_man
	    gen_server:cast(worker_pool_man,{add,FreePids});
	{error,Reason} ->
	    %%Log this event and find out why the file is not open,
	    %%Should we retry?
	    io:format("Can not open file, reason ~p~n",[Reason]),
	    ok
    end.
