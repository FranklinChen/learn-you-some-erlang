-module(zoo).
-export([main/0]).

-type red_panda() :: bamboo | birds | eggs | berries.
-type squid() :: sperm_whale.
-type food(A) :: fun(() -> A).

-spec feeder(red_panda) -> food(red_panda());
            (squid) -> food(squid()).
feeder(red_panda) ->
    fun() ->
        element(random:uniform(4), {bamboo, birds, eggs, berries})
    end;
feeder(squid) ->
    fun() -> sperm_whale end.

-spec feed_red_panda(food(red_panda())) -> red_panda().
feed_red_panda(Generator) ->
    Food = Generator(),
    io:format("feeding ~p to the red panda~n", [Food]),
    Food.

-spec feed_squid(food(squid())) -> squid().
feed_squid(Generator) ->
    Food = Generator(),
    io:format("feeding ~p to the squid~n", [Food]),
    Food.

main() ->
    %% Random seeding
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    %% The zoo buys a feeder for both the red panda and squid
    FeederRP = feeder(red_panda),
    FeederSquid = feeder(squid),
    %% Time to feed them! If we do it correctly at least once,
    %% Then no warning ever happens. Comment the two lines
    %% below to enable dialyzer figuring stuff out! This is likely
    %% failing because Dialyzer sees both calls as valid and thus
    %% needs not to reevaluate them again.
    %feed_squid(FeederSquid),
    %feed_red_panda(FeederRP),
    %% This should not be right!
    feed_squid(FeederRP),
    feed_red_panda(FeederSquid).
