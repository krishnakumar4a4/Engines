%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, %%{url,Pid}
	       count %%Count of the Url hits
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
    %%Many workers may exist, so dont name them
    gen_server:start_link(?MODULE, [], []).

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
handle_call({register,{Url,Pid}}, _From, State) ->
    %%Register this worker with some Url
    {reply, registered, State#state{name = {Url,Pid},count=1}};
handle_call({just_discard},_From,State) ->
    %% State doesnt matter for me,just discard it
    {stop,normal,State};
handle_call({commit_discard},_From,State) ->
    %%Commit to the database and stop this worker
    {reply,done_and_stop,State};
handle_call({discard,{Url,Pid}}, _From, State) when {Url,Pid}
						    =:= State#state.name->
    %%Discard the worker process when not necessary by events manager
    {stop,normal,State};
handle_call({commit,{Url,Pid}}, _From, State) when Pid=:=self()->
    %%Update persistent database with the count
    %%After the commit, this process can be reused,why dont we 
    %%contact worker_pool_man for reuse
    {reply,{Url,Pid,State#state.count},State};
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
handle_cast({incr,Name}, State) when State#state.name
					      =:= Name->
    io:format("The count is now ~p at ~p,received ~p~n",[State#state.count + 1,self(),Name]),
    %%Increment the counter for a Url
    {noreply, State#state{count = State#state.count + 1}};
handle_cast(_Msg,State) ->
    {noreply,State}.


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
