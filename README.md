# Book code for [Learn You Some Erlang for Great Good!](http://learnyousomeerlang.com/)

Downloaded from the [official Zip archive link](http://learnyousomeerlang.com/static/erlang/learn-you-some-erlang.zip) with the intention of maintaining as needed as Erlang evolves.

## Tests

```make test```

runs all the tests and saves a copy of the output to `test.log` for further inspection.

### Status

Currently, not all the tests pass under Erlang R16.

The following have problems:

```
	[{road_tests,[{main,1}]},
         {useless_tests,[{hello_test,0}]},
         {dolphins_tests,[{dolphin1,0}]},
         {functions_tests,[{valid_time_test,0}]}]
```
