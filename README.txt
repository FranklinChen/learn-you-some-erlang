Man there's not a lot to write in a readme for such a book.

You can get the license info in LICENSE.TXT (hint: it's the MIT license).
I probably won't copy it in every module. Just pretend it's there, please.

The code in these files has been tested with Erlang releases R13B+. In case you're not sure it will run on another version, you can run the tests yourself by compiling tester.erl and then calling:

1> tester:dir().
or
2> tester:dir("/modules/path","/unit/tests/modules/path/").

Which should run all the tests. If you're using the release R13B, there is a patch you need to apply in order to have unit tests running (see http://erlang.org/download.html).

Note that even if the tests run fine, you'd benefit in using releases R13B+.

If you need any more help, don't hesitate contacting me: mononcqc@ferd.ca.

