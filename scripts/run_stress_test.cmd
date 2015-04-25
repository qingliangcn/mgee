
cd ..\ebin
erlc -I ../include ../test/mgee_test_walk.erl

werl -boot start_sasl -config mgee_test_app.config  -eval "eunit:test(mgee_unittest_all, [verbose])" -s mgee_test_app start 



