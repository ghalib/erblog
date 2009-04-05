Not much a readme yet (app is still very much in development with
nearly-nonexistent documentation, but it does run after some effort).

For the eager ones, here are the steps to get it running:

1) Install Erlang
2) Install Distel (http://code.google.com/p/distel/)
3) Start erblog on remote server by running start.sh (look into this
file to see/modify startup params passed to erl)
4) Set remote nodename from Distel to your erblog nodename (test that
the connection is successful by hitting C-c C-d g in Emacs)
5) Load the file deps/elisp/blog.el
6) See file doc/examplepost.el for a barebones example (see all def
functions at the bottom of blog.el for complete list)
7) When you are ready to post, do M-x gms-test-html to make sure your
post results in valid HTML, then do an M-x gms-publish-post to publish
your blogpost






