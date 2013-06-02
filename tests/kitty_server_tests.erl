-module(kitty_server_tests).
-record(cat, {name, color=green, description}). % stolen from kitty_server.erl
-include_lib("eunit/include/eunit.hrl").
-define(CAT1, #cat{name=a, color=b, description=c}).
-define(CAT2, #cat{name=d, color=e, description=f}).

order_test() ->
    Pid = kitty_server:start_link(),
    ?assertEqual(?CAT1, kitty_server:order_cat(Pid, a, b, c)),
    ?assertEqual(?CAT2, kitty_server:order_cat(Pid, d, e, f)),
    ?assertEqual(ok, kitty_server:close_shop(Pid)).

return_test() ->
    Pid = kitty_server:start_link(),
    ?assertEqual(ok, kitty_server:return_cat(Pid, ?CAT1)),
    ?assertEqual(?CAT1, kitty_server:order_cat(Pid, d, e, f)),
    ?assertEqual(?CAT2, kitty_server:order_cat(Pid, d, e, f)),
    ?assertEqual(ok, kitty_server:close_shop(Pid)).

close_noproc_test() ->
    DeadPid = spawn_link(fun() -> ok end),
    timer:sleep(100),
    ?assertError(noproc, kitty_server:close_shop(DeadPid)).
