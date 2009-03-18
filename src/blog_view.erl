-module(blog_view).
-author('ghalib <ghalib@sent.com>').

-include("blogpost.hrl").

-compile(export_all).
%% -export([about_page/0,
%% 	blog_page/0,]).

-define(NAVLINKS, ["about", "code", "blog"]).


active_class(Link, ActiveLink) ->
    if Link =:= ActiveLink ->
	    "active";
       true ->
	    ""
    end.


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
    blog_util:html_text({html, [],
			 [{head, [],
			   [{link, [{rel, 'stylesheet'},
				    {type, 'text/css'},
				    {href, '/style.css'}], 
			     []}]},
			  {body, [],
			   [make_navbar(ActiveLink) | Content]}]}).

about_page() ->
    make_page("about",
	      {'div', [{id, 'text'}],
	       about_text()}).
 
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
		format_all_blogposts()}]).

blog_page(Permalink) ->
    make_page("blog",
	      {'div', [{id, 'text'}],
	       [format_blogpost(blog_db:get_blogpost(Permalink))]}).

%% @doc Called on a blog post's HTML before it is added to the
%% database (i.e. at the beginning of blog_db:add_blogpost/3), making
%% sure it is well-formed.  Delegates work to HTML tree creation to
%% mochiweb_html:to_html, in the sense that if the HTML is
%% badly-formed, the function will throw an exception.
test_html(HTMLBody) ->
    blog_util:html_text({html, [],
			 [{head, [], []},
			  {body, [], HTMLBody}]}).
