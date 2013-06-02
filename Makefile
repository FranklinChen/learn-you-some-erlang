# Run all the unit tests
test:	tester.beam
	erl -noshell -s tester dir -s init stop | tee test.log

%.beam:	%.erl
	erlc $^
