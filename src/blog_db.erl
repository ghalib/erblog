%%%-------------------------------------------------------------------
%%% File    : blog_db.erl
%%% Author  : ghalib <>
%%% Description : 
%%%
%%% Created : 16 Feb 2009 by ghalib <>
%%%-------------------------------------------------------------------
-module(blog_db).
-include_lib("stdlib/include/qlc.hrl").
-include("blogpost.hrl").

-behaviour(gen_server).

%% API
-export([start/0, stop/0]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

add_blogpost(Title, Body) ->
    gen_server:call(?MODULE, {add, Title, Body}).

get_blogpost(Permalink) ->
    gen_server:call(?MODULE, {get, Permalink}).

delete_blogpost(Permalink) ->
    gen_server:call(?MODULE, {delete, Permalink}).

blogposts() ->
    gen_server:call(?MODULE, allposts).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Posts = get_all_posts(),
    {ok, Posts}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, Title, Body}, _From, Posts) ->
    {Reply, NewPosts} = add_post(Title, Body, Posts),
    {reply, Reply, NewPosts};

handle_call({get, Permalink}, _From, Posts) ->
    {reply, get_post(Permalink), Posts};

handle_call({delete, Permalink}, _From, _Posts) ->
    Reply = delete_post(Permalink),
    NewPosts =  get_all_posts(),
    {reply, Reply, NewPosts};

handle_call(allposts, _From, Posts) ->
    Reply = Posts,
    {reply, Reply, Posts}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    start(),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% FIXME: Currently this will overwrite an existing entry sharing the
%% same permalink as the new one.
add_post(Title, Body, Posts) ->
    blog_view:test_html(Body),
    Permalink = blog_util:canonicalise(Title),
    Post = #blogpost{timestamp = term_to_binary(calendar:local_time()),
		     permalink = list_to_binary(Permalink),
		     title = list_to_binary(Title),
		     body = term_to_binary(Body)},
    Reply = blog_util:database_write(Post),
    {Reply, [Post | Posts]}.

get_post(Permalink) ->
    Result = blog_util:database_read(blogpost, list_to_binary(Permalink)),
    case Result of
	[] ->
	    false;
	[Post] ->
	    Post
    end.

delete_post(Permalink) ->
    blog_util:database_delete(blogpost, Permalink).

get_all_posts() ->
    Posts = blog_util:do_query(qlc:q([Post || Post <- mnesia:table(blogpost)])),
    lists:sort(fun(Post1, Post2) -> 
		       binary_to_term(Post1#blogpost.timestamp) > binary_to_term(Post2#blogpost.timestamp) end,
	       Posts).

