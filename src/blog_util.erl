-module(blog_util).
-author('ghalib@sent.com').
-include("blogpost.hrl").

-export([redirect/2,
	 length_1/1,
	 sum/1,
	 integers_to_lists/1,
	 database_read/2,
	 database_write/1,
	 database_delete/2,
	 but_last/1,
	 gen_unique_permalink/1,
	 pretty_time/1,
	 do_query/1,
	 print_post/1,
	 capitalise/1,
	 init_db/0
	]).

%% From mochiweb_html.erl:
%% @type html_node() = {string(), [html_attr()], [html_node() | string()]}
%% @type html_attr() = {string(), string()}
%% @type html_token() = html_data() | start_tag() | end_tag() | inline_html() | html_comment() | html_doctype()

redirect(Req, Url) ->
    Req:respond({302,
		 [{"Location", Url},
		  {"Content-Type", "text/html; charset=UTF-8"}],
		 ""}).

%% @spec length_1(list()) -> bool()
%% @doc Is the list of length 1?
length_1([_H]) ->
    true;
length_1([_H | _T]) ->
    false.

%% @spec sum(list()) -> number()
%% @doc Sums up a list of numbers
sum(L) ->
    sum(L, 0).
sum([], Acc) ->
    Acc;
sum([H | T], Acc) ->
    sum(T, Acc + H).


%% @spec integers_to_lists(list()) -> list() 
%% @doc Converts list of integers to list of string representation of
%% said integers.
integers_to_lists(Integers) ->
    lists:map(fun integer_to_list/1, Integers).


%% @spec put_zero(string()) -> string()
%% @doc Prepends a "0" if Integer is a single digit. e.g. "8" -> "08"
put_zero(Integer) ->
    case length_1(Integer) of
	true ->
	    "0" ++ Integer;
	_ ->
	    Integer
    end.


%% @spec pretty_time({date(), time()}) -> string()

%% @doc Takes time tuple returned by calendar:local_time() and returns
%% a more palatable string representation. 
%% e.g. {{2009,2,10},{23,51,58}} -> 23:51:58 2009/02/10
pretty_time(LocalTime) ->
    {Dt, Tm} = LocalTime,
    [Date, Time] = lists:map(fun(Time_Elem) -> 
				     lists:map(fun put_zero/1,
					       blog_util:integers_to_lists(tuple_to_list(Time_Elem))) end, 
			     [Dt, Tm]),
        
    string:join(Time, ":") ++ " " ++ string:join(Date, "/").
    

%% db functions

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

%% @spec but_last(list()) -> list()
%% @doc Return all elements of list except last
but_last(L) ->
    but_last(L, []).

but_last([_ | []], Acc) ->
    lists:reverse(Acc);
but_last([H|T], Acc) ->
    but_last(T, [H | Acc]).
    
%% Replace all spaces in title with one dash (-), to make it more
%% representable when permalinking.
canonicalise(Title) ->
    Title2 = if 
		 is_binary(Title) ->
		     binary_to_list(Title);
		 true ->
		     Title
	     end,
    Tokens = lists:sublist(string:tokens(string:to_lower(Title2), " "), 4),
    string:join(Tokens, "-").
    
gen_unique_id() ->
    TimeHash = crypto:sha(term_to_binary(now())),
    sum(binary_to_list(TimeHash)).

gen_unique_permalink(Title) ->
    %% Check if this already exists in db.  If no, we are done.
    %% Otherwise, generate unique ID (see gen_unique_id/0) and append
    %% this ID to title.
    CanonicalTitle = canonicalise(Title),
    
    Permalink = case blog_db:get_post(CanonicalTitle) of
		    false ->
			CanonicalTitle;
		    _Post ->
			CanonicalTitle ++ integer_to_list(gen_unique_id())
		end,
    Permalink.
    
%% Taken from jaerlang
do_query(Query) ->
    F = fun() ->
		qlc:e(Query) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% debugging
print_post(Blogpost) ->
    Blogpost#blogpost.permalink ++ "<br/>" ++
	Blogpost#blogpost.title ++ "<br/>" ++
	Blogpost#blogpost.body ++ "<br/>" ++
	blog_util:pretty_time(Blogpost#blogpost.timestamp).

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


capitalise(Word) ->
    [string:to_upper(hd(Word)) | tl(Word)].
