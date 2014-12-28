{application, http_interface,
 [{description, "HTTP rest interface for simple_cache using gen_web_server"},
  {vsn, "0.1.0"},
  {modules, [hi_app,
	     hi_server,
	     hi_sup]},
  {registered, [hi_sup]},
  {applications, [kernel, stdlib, simple_cache]},
  {mod, {hi_app, []}}
 ]}.
