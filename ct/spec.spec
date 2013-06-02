{alias, demo, "./demo/"}.
{alias, meeting, "./meeting/"}.
{logdir, "./logs/"}.

{suites, meeting, all}.
{suites, demo, all}.
{skip_cases, demo, basic_SUITE, test2, "This test fails on purpose"}.
