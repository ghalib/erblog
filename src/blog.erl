%% @author Ghalib Suleiman <ghalib@sent.com>
%% @copyright 2008 Ghalib Suleiman.

%% @doc TEMPLATE.

-module(blog).
-author('ghalib <ghalib@sent.com>').

-export([start/0, stop/0]).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the blog server.
start() ->
    blog_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(blog),
    blog_db:start().

%% @spec stop() -> ok
%% @doc Stop the blog server.
stop() ->
    blog_db:stop(),
    Res = application:stop(blog),
    application:stop(mnesia),
    application:stop(crypto),
    Res.
