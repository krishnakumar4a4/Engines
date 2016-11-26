%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(events).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/1]).
-export([start_mgr/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {worker_pool_caliberate_thres, %%This is used
		%%to caliberate the worker_pool strategy based
		%%unique events size
		ets_tab_threshold = 2000, %%Max size we would 
		%%allow one table to grow
		batcher_pid, %%Pid of the batcher server
		current_table,
		current_size,
		next_table,
		worker_pool_manager 
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%Start an event manager to receive all the events
start_mgr() ->
  
    case ?MODULE:start_link() of
	{ok,EPid} ->
	    %%Unique events cache table
	    Tid = ets:new(events,[ordered_set,{read_concurrency,true}]), 
	    %%Take access on the table
	    ets:give_away(Tid,EPid,[]),
	    %%Add a handler that handles url likes
	    ?MODULE:add_handler(Tid),
	    %%Started the batcher
	    batcher:start_link(),
	    EPid;
	{error,Reason} ->
	    Reason
    end.
%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Tid) ->
    gen_event:add_handler(?SERVER, ?MODULE, [Tid]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Tid]) ->
    %%Spin out a worker pool manager
    case worker_pool_man:start_link([{init_tab_size,100}]) of
	{ok,Pid} ->
	    case batcher:start_link() of
		{ok,BatPid}->
		    {ok, #state{worker_pool_caliberate_thres = 100,
				worker_pool_manager = Pid,
				batcher_pid = BatPid,
				current_table = Tid,
			       current_size = 2000}};
		{already_started,BatPid} ->
		    {ok, #state{worker_pool_caliberate_thres = 100,
				worker_pool_manager = Pid,
				batcher_pid = BatPid,
			       current_table = Tid,
				current_size = 2000}};
		{error,Reason} ->
		    %%This is a big problem let the authorities know 
		    %%about it,raise a siren
		    {stop,Reason}
	    end;
	{already_started,Pid} ->
	    %%Oh, if already started,let us sync the worker pool
	    %%strategy!!!!!!!!!!!!!!!!!!!!!!!!!
	    {ok, #state{worker_pool_caliberate_thres = 100,
		       current_table = Tid,
		       worker_pool_manager = Pid,
			current_size = 2000}};
	{error,Reason} ->
	    %%This is a big problem let the authorities know 
	    %%about it,raise a siren
	    {stop,Reason}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info({like_url,URL}, State) ->
    CurrTab = State#state.current_table,
    case ets:lookup(CurrTab,URL) of
	[{URL,WorkPid}] ->
	    %%Send an increment message to the PID
	    gen_server:cast(WorkPid,{incr,{URL,WorkPid}}),
	    {ok, State};
	[] ->
	    %%spawns the registration process, if registration
	    %%never happening,raise an alarm
	    %%spawn(?MODULE,register_with_new_worker,[URL,self()]),
	    register_with_new_worker(URL,self(),CurrTab),
	    CurrSize = State#state.current_size - 1,
	    case CurrSize of
		10 ->
		    Tid = ets:new(events,[ordered_set,{read_concurrency,true}]), 
		    io:format("creating new events table ~p~n",[Tid]),
		    {ok,State#state{current_size = 10,
				    next_table = Tid}};
		0 ->
		    %%Take access on the table
		    ets:give_away(CurrTab,State#state.batcher_pid,[]),
		    io:format("Gave a table to batcher~n"),
		    gen_server:cast(batcher,
				    {takeover,State#state.current_table}),
		    {ok,State#state{current_size = 
					State#state.ets_tab_threshold,
				    current_table = State#state.next_table}};
		CurrSize ->
		    {ok,State#state{current_size = CurrSize}}
	    end
    end;
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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

register_with_new_worker(Url,EventManPid,TabId) ->
    NewWorkerPid = worker_pool_man:get_me_worker(),
    case gen_server:call(NewWorkerPid,{register,
				  {Url,NewWorkerPid}},
			 infinity) of
	registered ->
	    %%io:format("inserting into db"),
	    ets:insert(TabId,{Url,NewWorkerPid}),
	    %%io:format("~nlist:~p~n",[ets:tab2list(events)]),
	    ok;
	_ ->
	    io:format("demolishing pid:~p~n",[NewWorkerPid]),
	    worker_pool_pid:demolish_worker(NewWorkerPid),
	    register_with_new_worker(Url,EventManPid,TabId)
    end.
