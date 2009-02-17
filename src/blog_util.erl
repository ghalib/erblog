-module(blog_util).
-author('ghalib@sent.com').
-include("blogpost.hrl").

-compile(export_all).

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
    

%% @spec html_text([html_token()] | html_node()) -> binary()
%% @doc Convert a list of html_token() to an HTML document in binary form.
html_text(Mochihtml) ->
    iolist_to_binary(mochiweb_html:to_html(Mochihtml)).


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

%% Replace all spaces in title with one dash (-), to make it more
%% representable when permalinking.
canonicalise(Title) ->
    Tokens = lists:sublist(string:tokens(string:to_lower(Title), " "), 4),
    string:join(Tokens, "-").
    
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


