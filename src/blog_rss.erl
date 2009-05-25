-module(blog_rss).
-author('ghalib@sent.com').

-include("blogpost.hrl").

-export([rss_page/0]).

make_item(Blogpost) ->
    {item, [], 
     [{title, [], Blogpost#blogpost.title},
      {description, [], ""},
      {link, [], iolist_to_binary([<<"http://www.ghalib.me/blog/">>, 
				   Blogpost#blogpost.permalink])}]}.

make_items(Blogposts) ->
    lists:map(fun make_item/1, Blogposts).

make_channel(Items) ->
    {channel, [], Items}.

rss_tagtree() ->
    {rss, [{version, <<"2.0">>}], [make_channel(make_items(blog_db:blogposts()))]}.

rss_page() ->
    iolist_to_binary([<<"<?xml version=\"1.0\"?>">>, 
		      mochiweb_html:to_html(rss_tagtree())]).
    
