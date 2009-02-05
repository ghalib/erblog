%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the blog application.

-module(blog_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for blog.
start(_Type, _StartArgs) ->
    blog_deps:ensure(),
    blog_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for blog.
stop(_State) ->
    ok.
