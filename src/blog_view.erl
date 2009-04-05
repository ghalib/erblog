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
    iolist_to_binary(mochiweb_html:to_html(doctype()) ++ 
		     mochiweb_html:to_html(Mochihtml)).

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

disqus_embed() ->
    [{'div', [{id, 'disqus_thread'}], []},
     {script, [{type, 'text/javascript'},
	       {src, 'http://disqus.com/forums/ghalib/embed.js'}], []},
     {noscript, [], [{a, [{href, 'http://ghalib.disqus.com/?url=ref'}],
		      <<"View the discussion thread.">>}]},
     {a, [{href, 'http://disqus.com'},
	  {class, 'dsq-brlink'}],
      [<<"blog comments powered by ">>, {span, [{class, 'logo-disqus'}],
					 <<"Disqus">>}]}].

active_class(Link, ActiveLink) ->
    if Link =:= ActiveLink ->
	    "active";
       true ->
	    ""
    end.

html_deflist(Items) ->
    {dl, [], lists:flatten(lists:map(fun({Item, Def}) ->
					     [{dt, [], [Item]},
					      {dd, [], Def}] end,
				     Items))}.    

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
    Sulaiman). I graduated from university in December 2006. Everyone
    else seems to have a website, so I thought I would hop on the
    bandwagon too.">>},

     {p, [], [<<"If you are doing any interesting work, I would love to hear from you. I can be reached at ">>, 
	      {a, [{href, 'mailto:ghalib@sent.com'}], <<"ghalib@sent.com">>},
	      <<".">>]},
    
     copyright()].

copyright() ->
    {'div', [{id, 'copyright'}],
     <<"© Ghalib Suleiman 2009">>}.

make_page(ActiveLink, Content) ->
    html_text({html, [{xmlns, <<"http://www.w3.org/1999/xhtml">>},
		      {'xml:lang', <<"en">>},
		      {lang, <<"en">>}],
	       [{head, [],
		 [{link, [{rel, 'stylesheet'},
			  {type, 'text/css'},
			  {href, '/style.css'}], 
		   []},
		  {title, [], <<"Ghalib Suleiman">>}]},
		{body, [],
		 lists:flatten([make_navbar(ActiveLink), Content, 
			       google_analytics_embed()])}]}).

about_page() ->
    make_page("about",
	      [{'div', [{id, 'text'}],
	       about_text()}]).

code_listing() ->
    html_deflist([
		  {html_link(<<"/files/bloom.py">>, <<"bloom.py">>),
		   [<<"A Bloom filter written in Python. A nice overview on Bloom filters can be found ">>, 
		   html_link(<<"http://www.internetmathematics.org/volumes/1/4/Broder.pdf">>, <<"here">>),
		    <<" (warning: PDF).">>]},

		  {html_link(<<"/files/scheme.tar.gz">>, <<"Arithmetic Scheme">>),
		   [<<"A toy Scheme calculator written in C++. Basically a Scheme interpreter where the only functions are basic arithmetic. A company I once applied to work for asked me for a C++ code sample that was more than 1k lines of code. I didn't have anything on me as I don't normally write C++ at home, so I quickly whipped this up.">>]},
		  
		  {html_link(<<"/files/sicp-solutions.tar.gz">>, <<"SICP solutions">>),
		   [<<"Solutions to the timeless ">>, html_link(<<"http://www.amazon.com/dp/0262011530/">>, <<"SICP">>), <<" (up to half of chapter 4). I haven't touched these in nearly two years; I got to the AMB evaluator section and got distracted with other things. Haven't gone back since, but hopefully will one day.">>]},
		  
		  {html_link(<<"/files/.emacs">>, <<".emacs">>),
		   [<<"My Emacs startup file.">>]}
		 ]).

code_page() ->
    make_page("code",
	      {'div', [{id, 'text'}],
		       [code_listing()]}).
		
 
format_blogpost(Blogpost) ->
    {'div', [{class, 'blogpost'}],
     [{h3, [{class, 'blogtitle'}],
       [Blogpost#blogpost.title]},
      {p, [], 
       binary_to_term(Blogpost#blogpost.body)},
      {'div', [{class, 'blogfooter'}],
       [{h5, [{class, 'footerlink'}],
	 [{a, [{href, "/blog/" ++ binary_to_list(Blogpost#blogpost.permalink)}],
	   [<<"Link to this post">>]}]},
	{h5, [{class, 'footerdate'}],
	 [blog_util:pretty_time(binary_to_term(Blogpost#blogpost.timestamp))]}]}]}.

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
    html_list(TitleLinks, 'archivelist').

blog_page() ->
    make_page("blog",
	      [{a, [{href, '/blog/archives'},
		    {class, 'navlink archives'}], <<"Archives">>},
	       {'div', [{id, 'text'}],
		[format_blogpost(blog_db:most_recent_blogpost()) |
		 disqus_embed()]}]).

blog_page(Permalink) ->
    make_page("blog",
	      [{a, [{href, '/blog/archives'},
		    {class, 'navlink archives'}], <<"Archives">>},
	       {'div', [{id, 'text'}],
	       [format_blogpost(blog_db:get_blogpost(Permalink))]}]).

archives_page() ->
    make_page("", 
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
