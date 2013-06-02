{node, master, 'ct@li101-172.members.linode.com'}.
{node, a, 'a@li101-172.members.linode.com'}.
{node, b, 'b@li101-172.members.linode.com'}.

{init, a, [{node_start, [{monitor_master, true},
                         {boot_timeout, 10000},
                         {erl_flags, "-pa /home/ferd/code/learn-you-some-erlang/m8ball/ebin/ "
                                     "-config /home/ferd/code/learn-you-some-erlang/m8ball/test/dist_m8ball_SUITE_data/main.config"}]}]}.
{init, b, [{node_start, [{monitor_master, true},
                         {boot_timeout, 10000},
                         {erl_flags, "-pa /home/ferd/code/learn-you-some-erlang/m8ball/ebin/ "
                                     "-config /home/ferd/code/learn-you-some-erlang/m8ball/test/dist_m8ball_SUITE_data/backup.config"}]}]}.

{include, "../include/"}.
{alias, root, "/home/ferd/code/learn-you-some-erlang/m8ball/test"}.
{logdir, "/home/ferd/code/learn-you-some-erlang/m8ball/logs"}.
{logdir, master, "/home/ferd/code/learn-you-some-erlang/m8ball/logs"}.

{groups, a, root, dist_m8ball_SUITE, main}.
{groups, b, root, dist_m8ball_SUITE, backup}.
