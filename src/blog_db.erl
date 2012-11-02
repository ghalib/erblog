%%%-------------------------------------------------------------------
%%% File    : blog_db.erl
%%% Author  : ghalib <ghalib@sent.com>
%%% Description : 
%%%
%%% Created : 16 Feb 2009 by ghalib <ghalib@sent.com>
%%%-------------------------------------------------------------------
-module(blog_db).
-author('ghalib <ghalib@sent.com>').

-include_lib("stdlib/include/qlc.hrl").
-include("blogpost.hrl").

-behaviour(gen_server).

%% API
-export([start/0,
	 stop/0,
	 add_blogpost/2,
	 add_blogpost/3,
	 get_blogpost/1,
	 get_post/1,
	 most_recent_blogpost/0,
	 delete_blogpost/1,
	 blogposts/0]).
	 

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

add_blogpost(Title, Body, Permalink) ->
    gen_server:call(?MODULE, {add, Title, Body, Permalink}).

get_blogpost(Permalink) ->
    gen_server:call(?MODULE, {get, Permalink}).

most_recent_blogpost() ->
    gen_server:call(?MODULE, {most_recent}).

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
    {Reply, Post} = add_post(Title, Body),
    {reply, Reply, [Post | Posts]};

handle_call({add, Title, Body, Permalink}, _From, Posts) ->
    {Reply, Post} = add_post(Title, Body, Permalink),
    {reply, Reply, [Post | Posts]};

handle_call({get, Permalink}, _From, Posts) ->
    {reply, get_post(Permalink), Posts};

handle_call({most_recent}, _From, Posts) ->
    {reply, hd(Posts), Posts};

handle_call({delete, Permalink}, _From, _Posts) ->
    Reply = delete_post(Permalink),
    NewPosts =  get_all_posts(),
    {reply, Reply, NewPosts};

handle_call(allposts, _From, Posts) ->
    {reply, Posts, Posts}.
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

add_post(Title, Body) ->
    Permalink = blog_util:gen_unique_permalink(Title),
    add_post(Title, Body, Permalink).
    
add_post(Title, Body, Permalink) ->
    blog_view:test_html(Body),
    Post = #blogpost{timestamp = term_to_binary(calendar:local_time()),
		     permalink = list_to_binary(Permalink),
		     title = list_to_binary(Title),
		     body = term_to_binary(Body)},

    Reply = blog_util:database_write(Post),
    {Reply, Post}.

delete_post(Permalink) ->
    blog_util:database_delete(blogpost, list_to_binary(Permalink)).

get_post(Permalink) ->
    Result = blog_util:database_read(blogpost, list_to_binary(Permalink)),
    case Result of
	[] ->
	    false;
	[Post] ->
	    Post
    end.

get_all_posts() ->
    Posts = blog_util:do_query(qlc:q([Post || Post <- mnesia:table(blogpost)])),
    lists:sort(fun(Post1, Post2) -> 
		       binary_to_term(Post1#blogpost.timestamp) > binary_to_term(Post2#blogpost.timestamp) end,
	       Posts).

