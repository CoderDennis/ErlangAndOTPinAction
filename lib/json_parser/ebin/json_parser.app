{application, json_parser,
 [{description, "Wraps c library for parsing json data."},
  {vsn, "0.1.0"},
  {modules, [
	     json_parser,
	     jp_app,
	     jp_sup,
	     jp_server
	    ]},
  {registered, [jp_server]},
  {applications, [kernel, stdlib]},
  {mod, {jp_app, []}}
 ]}.
