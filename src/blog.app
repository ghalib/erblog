{application, blog,
 [{description, "Erblog"},
  {vsn, "0.2"},
  {modules, [
    blog,
    blog_app,
    blog_sup,
    blog_web,
    blog_deps,
    blog_db,
    blog_util,
    blog_view
  ]},
  {registered, []},
  {mod, {blog_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
