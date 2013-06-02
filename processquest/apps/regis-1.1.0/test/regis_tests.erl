-module(regis_tests).
-include_lib("eunit/include/eunit.hrl").

app_test_() ->
    {inorder,
     [?_assert(try application:start(regis) of
                 ok -> true;
                 {error, {already_started, regis}} -> true;
                 _ -> false
               catch
                 _:_ -> false
               end),
      ?_assert(try application:stop(regis) of
                 ok -> true;
                 _ -> false
               catch
                 _:_ -> false
               end)]}.
