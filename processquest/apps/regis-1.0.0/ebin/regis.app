{application, regis,
 [{description, "A non-distributed process registry"},
  {vsn, "1.0.0"},
  {mod, {regis, []}},
  {registered, [regis_server]},
  {modules, [regis, regis_sup, regis_server]},
  {applications, [stdlib, kernel]}]}.
