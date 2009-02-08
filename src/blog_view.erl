-module(blog_view).
-author('ghalib <ghalib@sent.com').

-export([about_page/0]).


make_topbar(Links) ->
    {'div', [{id, 'topbar'}],
     lists:map(fun({Link, Text, Classes}) ->
		       {a, [{href, Link},
			    {class, "navlink " ++ Classes}],
			Text} end,
	       Links)}.

about_text() -> 
    [{p, [], <<"Hello, my name is Ghalib Suleiman (or
    Sulaiman). I graduated from university in December 2006.">>},

     {p, [], <<"Everyone else seems to have a website, so I thought I
     would hop on the bandwagon too .">>}].

make_page(Content) ->
    blog_util:html_text({html, [],
			 [{head, [],
			   [{link, [{rel, 'stylesheet'},
				    {type, 'text/css'},
				    {href, 'style.css'}], 
			     []}]},
			  {body, [],
			   [make_topbar([{<<"about">>,
					  <<"About">>,
					  "current"},
					 {<<"code">>,
					  <<"Code">>,
					  ""},
					 {<<"blog">>,
					  <<"Blog">>,
					  ""}]),
			    Content]}]}).

about_page() ->
    make_page({'div', [{id, 'text'}],
	       about_text()}).
