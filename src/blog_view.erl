-module(blog_view).
-author('ghalib <ghalib@sent.com>').

-include("blogpost.hrl").

-compile(export_all).
-export([about_page/0,
	blog_page/0]).


make_topbar(Links) ->
    {'div', [{id, 'topbar'}],
     lists:map(fun({Link, Text, Classes}) ->
		       {a, [{href, Link},
			    {class, "navlink " ++ Classes}],
			Text} end,
	       Links)}.

about_text() -> 
    [{p, [], <<"Hello, my name is Ghalib Suleiman (or
    Sulaiman). I graduated from university in December 2006. Everyone
    else seems to have a website, so I thought I would hop on the
    bandwagon too.">>},

     {p, [], <<"If you are doing any interesting work, I would love to
     hear from you. I can be reached at <<ghalib ##at ##sent ^dot
     #com#>>">>}].

make_page(Topbar_Links, Content) ->
    blog_util:html_text({html, [],
			 [{head, [],
			   [{link, [{rel, 'stylesheet'},
				    {type, 'text/css'},
				    {href, 'style.css'}], 
			     []}]},
			  {body, [],
			   [make_topbar(Topbar_Links),
			    Content]}]}).

about_page() ->
    make_page([{<<"about">>, <<"About">>, "current"},
	       {<<"code">>, <<"Code">>,	""},
	       {<<"blog">>, <<"Blog">>,	""}],
	      {'div', [{id, 'text'}],
	       about_text()}).
 
format_blogpost(Blogpost) ->
    {'div', [{class, 'blogpost'}],
     [{'h3', [{class, 'blogtitle'}],
       [Blogpost#blogpost.title]},
      {p, [], 
       Blogpost#blogpost.body},
      {'div', [{class, 'blogfooter'}],
       [{h5, [{class, 'footerlink'}],
	 [{a, [{href, "blog/" ++ Blogpost#blogpost.permalink}],
	   [<<"Link to this post">>]}]},
	{h5, [{class, 'footerdate'}],
	 [blog_util:pretty_time(Blogpost#blogpost.timestamp)]}]}]}.

format_all_blogposts() ->
    Blogposts = blog_db:get_all_posts(),
    lists:map(fun format_blogpost/1, Blogposts).

blog_page() ->
    make_page([{<<"about">>, <<"About">>, ""},
	       {<<"code">>, <<"Code">>, ""},
	       {<<"blog">>, <<"Blog">>, "current"}],
	      {'div', [{id, 'text'}],
	       format_all_blogposts()}).

%% @doc Called on a blog post's HTML before it is added to the
%% database (i.e. at the beginning of blog_db:add_blogpost/3), making
%% sure it is well-formed.  Delegates work to HTML tree creation to
%% mochiweb_html:to_html, in the sense that if the HTML is
%% badly-formed, the function will throw an exception and
%% add_blogpost/3 will fail.
test_html(HTMLBody) ->
    blog_util:html_text({html, [],
			 [{head, [], []},
			  {body, [], HTMLBody}]}).
