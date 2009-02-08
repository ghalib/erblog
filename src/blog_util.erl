-module(blog_util).

-export([redirect/2,
	integers_to_lists/1,
	pretty_time/1,
	html_text/1]).

redirect(Req, Url) ->
    Req:respond({302,
		 [{"Location", Url},
		  {"Content-Type", "text/html; charset=UTF-8"}],
		 ""}).

integers_to_lists(Integers) ->
    lists:map(fun integer_to_list/1, Integers).

%% Takes time tuple returned by calendar:local_time() and returns a
%% more palatable string representation.
pretty_time(LocalTime) ->
    {Date, Time} = LocalTime,
    Time2 = blog_util:integers_to_lists(tuple_to_list(Time)),
    Date2 = blog_util:integers_to_lists(tuple_to_list(Date)),
    string:join(Time2, ":") ++ " " ++ string:join(Date2, "/").
    
html_text(Mochihtml) ->
    iolist_to_binary(mochiweb_html:to_html(Mochihtml)).
