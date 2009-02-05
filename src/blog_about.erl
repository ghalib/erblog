-module(blog_about).
-author('ghalib <ghalib@sent.com').

-export([about_page/0]).


make_topbar(Links) ->
    {'div', [{id, 'topbar'}],
     lists:map(fun({Id, Link, Text}) ->
		       {a, [{id, Id},
			    {href, Link},
			    {class, 'navlink'}],
			Text} end,
	       Links)}.

about_page() ->
    iolist_to_binary(
      mochiweb_html:to_html({html, [],
			     [{head, [],
			       [{link, [{rel, 'stylesheet'},
					{type, 'text/css'},
					{href, 'styles.css'}], 
				 []}]},
			      {body, [],
			       [make_topbar([{'about',
					      'ui.html', 
					       <<"About">>},
					     {'code',
					      'codeui.html', 
					      <<"Code">>},
					     {'blog',
					      'blogui.html',
					      <<"Blog">>}]),
				{'div', [{id, 'text'}],
				 [{p, [], 
				   <<"Hello, my name is Ghalib Suleiman (or Sulaiman). I like computer science, maths, and programming (although since I graduated from university, I unfortunately don't get to practise the former two as much). Everyone else seems to be putting their code online, so I thought I would also hop on the bandwagon. Eventually this will become a proper site and you will see various bits of Python, Lisp (CL and Elisp), and Erlang (perhaps C++ too). I just bought a new laptop, so all that stuff is hidden in the depths of my old one, waiting to be rescued.">>}]}]}]})).
