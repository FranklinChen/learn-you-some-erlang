{application, processquest,
 [{description, "Game inspired by the Progress Quest game (http://progressquest.com)"},
  {vsn, "1.1.0"},
  {mod, {processquest, []}},
  {registered, [pq_supersup]},
  {modules, [processquest, pq_stats, pq_enemy, pq_quest, pq_events,
             pq_player]},
  {applications, [stdlib, kernel, regis, crypto]}]}.
