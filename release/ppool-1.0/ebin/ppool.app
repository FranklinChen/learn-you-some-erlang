{application, ppool,
 [{vsn, "1.0.0"},
  {description, "Run and enqueue different concurrent tasks"},
  {modules, [ppool, ppool_serv, ppool_sup, ppool_supersup, ppool_worker_sup]},
  {applications, [stdlib, kernel]},
  {registered, [ppool]},
  {mod, {ppool, []}}
 ]}.
