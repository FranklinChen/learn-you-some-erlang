{node, a, 'a@ferdmbp.local'}.
{node, b, 'b@ferdmbp.local'}.

{init, [a,b], [{node_start, [{monitor_master, true}]}]}.

{alias, demo, "/Users/ferd/code/self/learn-you-some-erlang/ct/demo/"}.
{alias, meeting, "/Users/ferd/code/self/learn-you-some-erlang/ct/meeting/"}.

{logdir, [all_nodes,master], "/Users/ferd/code/self/learn-you-some-erlang/ct/logs/"}.

{suites, [b], meeting, all}.
{suites, [a], demo, all}.
{skip_cases, [a], demo, basic_SUITE, test2, "This test fails on purpose"}.
