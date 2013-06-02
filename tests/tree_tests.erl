-module(tree_tests).
-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    ?assert({node, 'nil'} =:= tree:empty()).

%% oh god this gets ugly
insert_test_() ->
    T1 = tree:insert(a, "a", tree:empty()),
    T2 = tree:insert(c, "c", T1),
    T3 = tree:insert(n, "n",
         tree:insert(z, "z",
         tree:insert(t, "t",
         tree:insert(x, "x", T2)))),
    [?_assertEqual(T1, {node, {a,"a",{node,nil},{node,nil}}}),
     ?_assertEqual(T2, {node, {a,"a",
                               {node,nil},
                               {node, {c,"c",{node,nil},{node,nil}}}}}),
     ?_assertEqual(T3, {node, {a,"a",
                               {node, nil},
                               {node, {c,"c",
                                      {node, nil},
                                      {node, {x,"x",
                                             {node, {t,"t",
                                                    {node, {n,"n",
                                                            {node,nil},
                                                            {node,nil}}},
                                                    {node, nil}}},
                                             {node, {z,"z",
                                                    {node,nil},
                                                    {node,nil}}}}}}}}})].
%% not as bad!
lookup_test_() ->
    T = tree:insert(x, "x",
        tree:insert(t, "t",
        tree:insert(z, "z",
        tree:insert(n, "n",
        tree:insert(c, "c",
        tree:insert(a, "a", tree:empty())))))),
    [?_assert({ok,"t"} == tree:lookup(t,T)),
     ?_assert(undefined == tree:lookup(21, T)),
     ?_assert(undefined == tree:lookup(a, tree:empty()))].

%% done with both insert and lookup
update_test_() ->
    T1 = tree:insert(x, "x",
         tree:insert(t, "t",
         tree:insert(z, "z",
         tree:insert(n, "n",
         tree:insert(c, "c",
         tree:insert(a, "a", tree:empty())))))),
    T2 = tree:insert(x, "X", T1),
    [?_assertEqual({ok, "x"}, tree:lookup(x, T1)),
     ?_assertEqual({ok, "X"}, tree:lookup(x, T2))].

has_value_test_() ->
    T1 = tree:insert(x, "x",
         tree:insert(t, "t",
         tree:insert(z, "z",
         tree:insert(n, "n",
         tree:insert(c, "c",
         tree:insert(a, "a", tree:empty())))))), 
    [?_assertEqual(true, tree:has_value("z", T1)),
     ?_assertEqual(false,tree:has_value("Z", T1))].
