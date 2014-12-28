{application, resource_discovery,
 [{description, "Resource discovery and sharing service"},
  {vsn, "0.1.0"},
  {modules, [
	     resource_discovery,
	     rd_app,
	     rd_sup
	    ]},
  {registered, [rd_sup]},
  {applications, [kernel, stdlib]},
  {mod, {rd_app, []}}
 ]}.
