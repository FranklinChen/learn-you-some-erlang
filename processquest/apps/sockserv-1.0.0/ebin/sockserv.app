{application, sockserv,
 [{description, "Socket server to forward ProcessQuest messages to a client"},
  {vsn, "1.0.0"},
  {mod, {sockserv, []}},
  {registered, [sockserv_sup]},
  {modules, [sockserv, sockserv_sup, sockserv_serv, sockserv_trans,
             sockserv_pq_events]},
  {applications, [stdlib, kernel, processquest]},
  {env,
    [{port, 8082}]}
 ]}.
