%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(worker_pool_man).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([get_me_worker/0,demolish_worker/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {last_event_man_sync, %%Time stamp of last
	       %%time event manager synced
		last_event_man_tab_size, %%Size of events table last known
		worker_pool_size,  %%Current worker pool size
		buffer_percent, %%10 percent of workers
		%%should be created beforehand to meet the demand
		buffer_workers, %%New workers are stored here
		worker_pool_sup %%Worker pool supervisor PID
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
start_link(InitArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitArgs, []).

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
init(InitArgs) ->
    LastSyncTime = try erlang:timestamp() 
		   catch _Err1:_Reas1 -> erlang:now() end,
    LastEventTabSize = try [Size||Size=proplists:get_value(init_tab_size,InitArgs),is_integer(Size)]
		       catch _Err2:_Reas2 -> 0 end,
    %%Putting a supervisor for worker processes may nit be good 
    %%idea as when some worker is dead,supervisor restarts it 
    %%with a new pid, we never know that new pid until the 
    %%new process sends back a message to us.
    case catch worker_pool_sup:start_link() of
	{ok,WorkPoolSupRef} ->
	    case catch worker_pool_sup:add(LastEventTabSize+10) of
		Status when is_list(Status) ->
		    %%We need to monitor the child worker processes
		    WorkerList = [Pid||{Res,Pid}<-Status,Res==ok],
		    {ok, #state{last_event_man_sync = LastSyncTime,
				last_event_man_tab_size = LastEventTabSize,
				%%Let us get the current worker pool status
				worker_pool_size = proplists:get_value(workers,
								       worker_pool_sup:count()),
			       buffer_percent = 10,
				buffer_workers = WorkerList,
				worker_pool_sup = WorkPoolSupRef
			       }};
		Error ->
		    %%This is a big problem let the authorities know 
		    %%about it,raise a siren
		    {stop,Error}
	    end;
	ErrorReason ->
	    %%This is a big problem let the authorities know 
	    %%about it,raise a siren
	    {stop,ErrorReason}
    end.

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
handle_call({get},_From,State) ->
    case State#state.buffer_workers of
	[] ->
	    %%Handle below case 
	    {ok,NewPid} = supervisor:start_child(worker_pool_sup,[]),
	    {reply,NewPid,State#state{buffer_workers = []}};
	[H|T] ->
	    {reply,H,State#state{buffer_workers = T}}
    end;

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
handle_cast({demolish,Pid},State) ->
    case worker_pool_sup:restart_pid(Pid) of
	{ok,NewPid} ->
	    %%We assume that this function is only called
	    %%after get_me_worker,hence old pid should be removed
	    {noreply,State#state{buffer_workers =
				     [NewPid|State#state.buffer_workers]}};
	undefined ->
	    {noreply,State}
    end;
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

get_me_worker() ->
    gen_server:call(worker_pool_man,{get},100).

demolish_worker(Pid) ->
    %%Asynchronous action as we dont bother about this pid anymore
    gen_server:cast(worker_pool_man,{demolish,Pid},100).
