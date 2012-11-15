%% @author ghalib <ghalib@sent.com>
%% @copyright 2008 Ghalib Suleiman.

%% @doc Web server for blog.

-module(blog_web).
-author('ghalib@sent.com').

-include("blogpost.hrl").

%% External API
-export([start/1, stop/0, loop/2]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
		"about" ->
		    html_reply(Req, blog_view:about_page());
		"code" ->
		    html_reply(Req, blog_view:code_page());
		"blog" ->
		    redirect_to_latest(Req);
		"blog/archives" ->
		    html_reply(Req, blog_view:archives_page());
		"blog/rss" ->
		    html_reply(Req, blog_rss:rss_page());
		[$b, $l, $o, $g, $/ | Permalink] ->
		    html_reply(Req, blog_view:blog_page(Permalink));
		"" ->
		    redirect_to_latest(Req);
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

redirect_to_latest(Req) ->
    Latest = blog_db:most_recent_blogpost(),
    blog_util:redirect(Req, "blog/" ++ binary_to_list(Latest#blogpost.permalink)).

html_reply(Req, Reply) ->
    Req:ok({"text/html", Reply}).
