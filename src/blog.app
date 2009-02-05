{application, blog,
 [{description, "blog"},
  {vsn, "0.01"},
  {modules, [
    blog,
    blog_app,
    blog_sup,
    blog_web,
    blog_deps
  ]},
  {registered, []},
  {mod, {blog_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
