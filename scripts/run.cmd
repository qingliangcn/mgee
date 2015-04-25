cd /d %0/..

cd ../ebin

werl -boot start_sasl -config mgee.config  -eval "eunit:test(mgee_unittest_all, [verbose])" -s mgee start


