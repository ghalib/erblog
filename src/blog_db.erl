-module(blog_db).

-include_lib("stdlib/include/qlc.hrl").
-include("blogpost.hrl").

-compile(export_all).

%% -export([init_db/0, add_blogpost/3, get_all_posts/0, print_post/1,
%% 	get_blogpost/1]).


%% Call this only once!
init_db() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    init_tables().

init_tables() ->
    mnesia:create_table(blogpost, [{type, set}, 
				   {attributes, record_info(fields, blogpost)},
				   {disc_copies, [node()]}]).

database_read(Table, Key) ->
    F = fun() ->
		mnesia:read({Table, Key})
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

database_write(Record) ->	    
    F = fun() ->
		mnesia:write(Record)
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

database_delete(Table, Key) ->
    F = fun() ->
		mnesia:delete({Table, Key})
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% Replace all spaces in title with one dash (-), to make it more
%% representable when permalinking.
canonicalise(Title) ->
    Tokens = lists:sublist(string:tokens(string:to_lower(Title), " "), 4),
    string:join(Tokens, "-").
    

add_blogpost(Title, Body) ->
    blog_view:test_html(Body),
    Post = #blogpost{timestamp = term_to_binary(calendar:local_time()),
		     permalink = list_to_binary(canonicalise(Title)),
		     title = list_to_binary(Title),
		     body = term_to_binary(Body)},
    database_write(Post).

get_blogpost(Permalink) ->
    [Post] = database_read(blogpost, list_to_binary(Permalink)),
    Post.

delete_blogpost(Permalink) ->
    database_delete(blogpost, Permalink).

%% Taken from jaerlang
do_query(Query) ->
    F = fun() ->
		qlc:e(Query) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

get_all_posts() ->
    do_query(qlc:q([Post || Post <- mnesia:table(blogpost)])).

%% debugging
print_post(Blogpost) ->
    Blogpost#blogpost.permalink ++ "<br/>" ++
	Blogpost#blogpost.title ++ "<br/>" ++
	Blogpost#blogpost.body ++ "<br/>" ++
	blog_util:pretty_time(Blogpost#blogpost.timestamp).


