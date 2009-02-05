-module(blog_db).

-include_lib("stdlib/include/qlc.hrl").
-include("blogpost.hrl").
-export([init_db/0, add_blogpost/3, get_all_posts/0, print_post/1,
	get_blogpost/1, prettify/1]).


%% Call this only once!
init_db() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    init_tables().

init_tables() ->
    mnesia:create_table(blogpost, [{type, ordered_set}, 
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

%% Replace all spaces in title with one dash (-), to make it more
%% representable when permalinking.
canonicalise(Title) ->
    string:join(string:tokens(string:to_lower(Title), " "), "-").
    
add_blogpost(Title, Author, Body) ->
    Post = #blogpost{permalink = canonicalise(Title), title = Title,
		     author = Author, body = Body,
		     timestamp = calendar:local_time()},
    database_write(Post).

get_blogpost(Permalink) ->
    [Post] = database_read(blogpost, Permalink),
    Post.

%% Taken from jaerlang
do_query(Query) ->
    F = fun() ->
		qlc:e(Query) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

get_all_posts() ->
    do_query(qlc:q([Post || Post <- mnesia:table(blogpost)])).

print_post(Blogpost) ->
    Blogpost#blogpost.permalink ++ "<br/>" ++
	Blogpost#blogpost.title ++ "<br/>" ++
	Blogpost#blogpost.author ++ "<br/>" ++
	Blogpost#blogpost.body ++ "<br/>" ++
	prettify(Blogpost#blogpost.timestamp).

%% Takes time tuple returned by calendar:local_time() and returns a
%% more palatable string representation.
prettify(Time) ->
    {{Year, Month, Day},
     {Hour, Minutes, Seconds}} = Time,
    [Date, Time2] = lists:map(fun(L) -> 
				      lists:map(fun integer_to_list/1, L)
					  end,
					[[Year, Month, Day], 
					 [Hour, Minutes, Seconds]]),
    string:join(Date, "/") ++ " " ++ string:join(Time2, ":").
