{application, mafiapp,
 [{description, "Help the boss keep track of his friends"},
  {vsn, "1.0.1"},
  {modules, [mafiapp, mafiapp_sup]},
  {applications, [stdlib, kernel, mnesia]}]}.
