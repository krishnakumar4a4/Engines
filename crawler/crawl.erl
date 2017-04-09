%%%-------------------------------------------------------------------
%%% @author 
%%% @copyright (C) 2017, krishnak
%%% @doc
%%%
%%% @end
%%% Created :  4 Mar 2017 by krishnak
%%%-------------------------------------------------------------------
-module(crawl).

-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Include lib


-define(SERVER, ?MODULE).

-record(state, {}).

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
    process_flag(trap_exit, true),
    timer:start(),
    %%setting up URL table
    ets:new(urlTab,[named_table,set,public]),
    %%setting up email table
    ets:new(emailTab,[named_table,set,public]),
    io:format("crawl initialized"),
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
handle_call({post,XML}, _From, State) ->
    io:format("Timer ~p~n",[gen_server:cast(crawl,start_timer)]),
    case crawl(XML) of
	{true,_} ->
	    print_and_reset(),
	    {reply, ok, State};
	{false,Reason} ->
	    {reply,Reason,State}
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
handle_cast(timer_start, State) ->
    timer:send_after(900000,"stop"),
    {noreply, State};
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
crawl(XML) ->
    try xmerl_scan:string(XML) of
	{XmlRec,_} ->
	    EmailRecList = xmerl_xpath:string("/dataset/emails/email",XmlRec),
	    UrlRecList = xmerl_xpath:string("/dataset/resources/url",XmlRec),
	    crawl_start(EmailRecList,UrlRecList)
    catch
	_Error:{_,Reason,_,_,_} ->
	    {false,Reason}
    end.
	    
crawl_start(EmailRecList,UrlRecList) ->
    email_crawl(EmailRecList),
    url_crawl(UrlRecList).

email_crawl(EmailRecList) ->
    UpdateFun = fun(Tab,Entry) ->
			try ets:update_counter(Tab,Entry,1) 
			of 
			    Num when is_integer(Num) -> ok
			catch
			    error:badarg ->
				ets:insert(Tab,{Entry,1});
			    Other ->
				io:format("Failed to load entry ~p with reason ~p~n",[Entry,Other])
			end
		end,
    EmailValidateFun = fun(EmailRec) when is_record(EmailRec,xmlElement)->
			       [EmailTextRec|_] = EmailRec#xmlElement.content,
			       EmailText = EmailTextRec#xmlText.value,
			       case string:tokens(EmailText,"@ ") of
				   [_User,Domain] when Domain=:="gripapp.com" 
						      orelse Domain =:= "grip.com" ->
				       {true,EmailText};
				   _Ignore ->
				       false
			       end
		       end,
    ExtractAndUpdate = fun(EmailRec,Tab) ->
			       case EmailValidateFun(EmailRec) of
				   {true,Email} ->
				       UpdateFun(Tab,Email);
				   false ->
				       ok
			       end
		       end,
    [ExtractAndUpdate(EmailRec,emailTab)||EmailRec<-EmailRecList].

url_crawl([]) ->
    ok;
url_crawl(UrlRecList) ->
    DownloadFun = fun(UrlRec) when is_record(UrlRec,xmlElement) ->
			  [UrlTextRec|_] = UrlRec#xmlElement.content,
			  UrlLink = UrlTextRec#xmlText.value,
			  case ets:lookup(urlTab,UrlLink) of
			      [] ->
				  ets:insert(urlTab,{UrlLink,1}),
				  download_xml(UrlLink);
			      _ ->
				  {false,error,repeated_url}
			  end
		  end,
    url_crawl_recursive(UrlRecList,DownloadFun).

url_crawl_recursive([],_)->
    {true,ok};
url_crawl_recursive([UrlRec|Rem],DownloadFun) ->
    receive 
	"stop" ->
	    io:format("Timeout"),
	    {false,timeout}
    after 0 ->
	    case DownloadFun(UrlRec) of
		{true,DownloadedXML} ->
		    case crawl(DownloadedXML) of
			{false,timeout} ->
			    {false,timeout};
			{true,_}->
			    url_crawl_recursive(Rem,DownloadFun)
		    end;
		{false,_Err,_Reason} ->
		   url_crawl_recursive(Rem,DownloadFun) 
	    end
    end.

download_xml(Url) ->
    try httpc:request(get, {Url, []}, [], []) of
	{ok,{{_,200,_},_,Data}} ->
	    %%io:format("Downloaded url ~p~n",[Url]),
	    {true,Data}
    catch
	Err:Reason ->
	    io:format("Skipping download of ~p~n",[Url]),
	    {false,Err,Reason}
    end.

print_and_reset() ->
    EmailList = ets:tab2list(emailTab),
    [io:format("~p  ->  ~p~n",[Email,Count])||{Email,Count}<-EmailList],
    ets:delete_all_objects(urlTab),
    ets:delete_all_objects(emailTab).
							  

    
		
