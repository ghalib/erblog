%% @author ghalib <ghalib@sent.com>
%% @copyright 2008 Ghalib Suleiman.

%% @doc Web server for blog.

-module(blog_web).
-author('ghalib@sent.com').

-export([start/1, stop/0, loop/2]).

%% External API

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
		    Req:ok({"text/html", blog_view:about_page()});
		"blog" ->
		    Req:ok({"text/html", blog_view:blog_page()});
		[$b, $l, $o, $g, $/ | Permalink] ->
		    Req:ok({"text/html", blog_view:blog_page(Permalink)});
		"" ->
		    blog_util:redirect(Req, "/blog");
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
