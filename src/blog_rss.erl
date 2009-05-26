-module(blog_rss).
-author('ghalib@sent.com').

-include("blogpost.hrl").

-export([rss_page/0]).

-compile(export_all).

%% For description, just take first tag element of the post's body.
make_item(Blogpost) ->
    iolist_to_binary([<<"<item>">>, 
		      <<"<title>">>,
		      Blogpost#blogpost.title,
		      <<"</title>">>,
		      <<"<description>">>,
		      iolist_to_binary(mochiweb_html:to_html(hd(binary_to_term(Blogpost#blogpost.body)))),
		      <<"</description>">>,
		      <<"<link>">>, 
		      <<"http://www.ghalib.me/blog/">>, 
		      Blogpost#blogpost.permalink,
		      <<"</link>">>,
		      <<"</item>">>]).

make_items(Blogposts) ->
    lists:map(fun make_item/1, Blogposts).

make_channel(Items) ->
    iolist_to_binary([<<"<channel>">>, Items, <<"</channel>">>]).

rss_page() ->
    iolist_to_binary([<<"<?xml version=\"1.0\"?>">>,
		      <<"<rss version=\"2.0\"">>, 
		      make_channel(make_items(blog_db:blogposts())),
		      <<"</rss>">>]).

