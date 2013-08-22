-module(blog_view).
-author('ghalib <ghalib@sent.com>').

-include("blogpost.hrl").

-export([about_page/0,
	 blog_page/0,
	 blog_page/1,
	 code_page/0,
	 archives_page/0,
	 test_html/1]).

-define(NAVLINKS, ["about", "code", "blog"]).

%% @spec html_text([html_token()] | html_node()) -> binary()
%% @doc Convert a list of html_token() to an HTML document in binary form.
html_text(Mochihtml) ->
    iolist_to_binary([mochiweb_html:to_html(doctype()),
		      mochiweb_html:to_html(Mochihtml)]).

doctype() ->
    {doctype,
     [<<"html">>, <<"PUBLIC">>,
      <<"-//W3C//DTD XHTML 1.0 Transitional//EN">>,
      <<"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">>]}.

google_analytics_embed() ->
    [{script, [{type, 'text/javascript'}],
     <<"var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));">>},
     {script, [{type, 'text/javascript'}],
      <<"try {var pageTracker = _gat._getTracker(\"UA-8061888-1\");
pageTracker._trackPageview();} catch(err) {}">>}].

active_class(Link, ActiveLink) ->
    if Link =:= ActiveLink ->
	    "active";
       true ->
	    ""
    end.

html_link(Address, Description) ->
    {a, [{href, Address}], Description}.

html_list(Items, Class) ->
    {ul, [{class, Class}], lists:map(fun(Item) ->
			       {li, [], [Item]} end,
		       Items)}.

make_navbar(ActiveLink) ->
    {'div', [{id, 'navbar'}],
     lists:map(fun(NavLink) ->
		       {a, [{href, "/" ++ NavLink},
			    {class, "navlink " ++ active_class(NavLink, ActiveLink)}],
			blog_util:capitalise(NavLink)} end,
	       ?NAVLINKS)}.

about_text() -> 
    [{p, [], <<"Hello, my name is Ghalib Suleiman (or
Sulaiman). I worked in natural language processing until August 2013,
when I decided to quit my job to move to San Francisco and explore new
opportunities.">>},

     {p, [], [<<"I can be reached using any of these fine services:">>,
	      contacts_list()]},
     copyright()].

contacts_list() ->
    html_list([html_link(<<"mailto:ghalib@sent.com">>, <<"Email">>),
	       html_link(<<"http://twitter.com/ghalib">>, <<"Twitter">>),
	       html_link(<<"http://github.com/ghalib">>, <<"GitHub">>),
               html_link(<<"http://www.linkedin.com/in/ghalibs">>, <<"LinkedIn">>)],
	     'list').

copyright() ->
    {'div', [{id, 'copyright'}],
     <<"© Ghalib Suleiman 2013">>}.

make_page(Title, ActiveLink, Content) ->
    html_text({html, [{xmlns, <<"http://www.w3.org/1999/xhtml">>},
		      {'xml:lang', <<"en">>},
		      {lang, <<"en">>}],
	       [{head, [],
		 [{link, [{rel, 'stylesheet'},
			  {type, 'text/css'},
			  {href, '/style.css'}], 
		   []},
		  {link, [{rel, 'alternate'},
			  {title, 'Ghalib\'s Blog RSS Feed'},
			  {type, 'application/rss+xml'},
			  {href, 'http://www.ghalib.me/blog/rss'}],
		   []},
		  {title, [], Title}]},
		{body, [],
		 lists:flatten([make_navbar(ActiveLink), Content, 
			       google_analytics_embed()])}]}).

about_page() ->
    make_page("About",
	      "about",
	      [{'div', [{id, 'text'}],
	       about_text()}]).


code_listing() ->
    {p, [], [<<"See ">>, 
	     html_link(<<"http://github.com/ghalib">>, 
		       <<"my GitHub page ">>), 
	     <<"for examples of recreational code I have written.">>]}.


code_page() ->
    make_page("Code",
	      "code",
	      {'div', [{id, 'text'}],
		       [code_listing()]}).
		
 
format_blogpost(Blogpost) ->
    [{'div', [{class, 'blogpost'}],
      [{h3, [{class, 'blogtitle'}],
       [Blogpost#blogpost.title]},
      {p, [], 
       binary_to_term(Blogpost#blogpost.body)},
      {'div', [{class, 'blogfooter'}],
       [{h5, [{class, 'footerlink'}],
	 [{a, [{href, "/blog/" ++ binary_to_list(Blogpost#blogpost.permalink)}],
	   [<<"Link to this post">>]}]},
	{h5, [{class, 'footerdate'}],
	 [blog_util:pretty_time(binary_to_term(Blogpost#blogpost.timestamp))]}]}]}].

%% For debugging
format_all_blogposts() ->
    Blogposts = blog_db:blogposts(),
    lists:map(fun format_blogpost/1, Blogposts).

format_all_titles() ->
    Blogposts = blog_db:blogposts(),
    TitleLinks = lists:map(fun(Blogpost) ->
				   html_link(Blogpost#blogpost.permalink,
					     Blogpost#blogpost.title) end,
			   Blogposts),
    html_list(TitleLinks, 'list').

blog_page() ->
    make_page(<<"Ghalib Suleiman">>,
	      "blog",
	      [{a, [{href, '/blog/archives'},
		    {class, 'navlink archives'}], <<"Archives">>},
	       {'div', [{id, 'text'}],
		format_blogpost(blog_db:most_recent_blogpost())}]).

blog_page(Permalink) ->
    Blogpost = blog_db:get_blogpost(Permalink),
    make_page(Blogpost#blogpost.title,
	      "blog",
	      [{a, [{href, '/blog/archives'},
		    {class, 'navlink archives'}], <<"Archives">>},
	       {'div', [{id, 'text'}],
		format_blogpost(blog_db:get_blogpost(Permalink))}]).

archives_page() ->
    make_page(<<"Blog Archives">>,
	      "", 
	      {'div', [{id, 'text'}],
	       [format_all_titles()]}).

%% @doc Called on a blog post's HTML before it is added to the
%% database (i.e. at the beginning of blog_db:add_blogpost/3), making
%% sure it is well-formed.  Delegates work to HTML tree creation to
%% mochiweb_html:to_html, in the sense that if the HTML is
%% badly-formed, the function will throw an exception.
test_html(HTMLBody) ->
    html_text({html, [],
	       [{head, [], []},
		{body, [], HTMLBody}]}).
