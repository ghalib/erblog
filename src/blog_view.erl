-module(blog_view).
-author('ghalib <ghalib@sent.com>').

-include("blogpost.hrl").

-compile(export_all).
%% -export([about_page/0,
%% 	blog_page/0,]).

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


make_navbar(ActiveLink) ->
    {'div', [{id, 'navbar'}],
     lists:map(fun(NavLink) ->
		       {a, [{href, "/" ++ NavLink},
			    {class, "navlink " ++ active_class(NavLink, ActiveLink)}],
			blog_util:capitalise_word(NavLink)} end,
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
		 [make_navbar(ActiveLink) | Content]}]}).

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
		   [<<"Solutions to the timeless ">>, html_link(<<"http://www.amazon.com/dp/0262011530/">>, <<"SICP">>), <<" (up to half of chapter 4). I haven't touched these in nearly two years; I got to the AMB evaluator section and got distracted with other things. Haven't gone back since, but hopefully will one day.">>]}
		 ]).

code_page() ->
    make_page("code",
	      [{'div', [{id, 'text'}],
		[code_listing()]}]).
		
 
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

format_all_blogposts() ->
    Blogposts = blog_db:blogposts(),
    lists:map(fun format_blogpost/1, Blogposts).

blog_page() ->
    make_page("blog",
	      [{'div', [{id, 'text'}],
		[format_blogpost(blog_db:most_recent_blogpost())]}]).

blog_page(Permalink) ->
    make_page("blog",
	      [{'div', [{id, 'text'}],
	       [format_blogpost(blog_db:get_blogpost(Permalink))]}]).

%% @doc Called on a blog post's HTML before it is added to the
%% database (i.e. at the beginning of blog_db:add_blogpost/3), making
%% sure it is well-formed.  Delegates work to HTML tree creation to
%% mochiweb_html:to_html, in the sense that if the HTML is
%% badly-formed, the function will throw an exception.
test_html(HTMLBody) ->
    html_text({html, [],
	       [{head, [], []},
		{body, [], HTMLBody}]}).
