-module(cases_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test_() ->
    [?_assertEqual([1], cases:insert(1,[])),
     ?_assertEqual([1], cases:insert(1,[1])),
     ?_assertEqual([1,2], cases:insert(1,[2]))].

beach_test_() ->
    [?_assertEqual('favorable', cases:beach({celsius, 20})),
     ?_assertEqual('favorable', cases:beach({celsius, 45})),
     ?_assertEqual('avoid beach', cases:beach({celsius, 46})),
     ?_assertEqual('avoid beach', cases:beach({celsius, 19})),
     ?_assertEqual('scientifically favorable', cases:beach({kelvin, 293})),
     ?_assertEqual('scientifically favorable', cases:beach({kelvin, 318})),
     ?_assertEqual('avoid beach', cases:beach({kelvin, 292})),
     ?_assertEqual('avoid beach', cases:beach({celsius, 319})),
     ?_assertEqual('favorable in the US',
                   cases:beach({fahrenheit, 68})),
     ?_assertEqual('favorable in the US',
                   cases:beach({fahrenheit, 113})),
     ?_assertEqual('avoid beach', cases:beach({fahrenheit, 67})),
     ?_assertEqual('avoid beach', cases:beach({fahrenheit, 114})),
     ?_assertEqual('avoid beach', cases:beach(cat))].
