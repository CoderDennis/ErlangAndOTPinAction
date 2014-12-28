{application, gen_web_server,
 [{description, "An OTP behavior that implements a simple http server"},
  {vsn, "0.1.0"},
  {modules, [gen_web_server,
	     gws_connection_sup,
	     gws_server]},
  {applications, [kernel, stdlib]}
 ]}.
