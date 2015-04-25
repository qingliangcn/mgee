
cd ..\ebin
del *.beam

cd ..
erl -make

cd ebin

werl -boot start_sasl -config mgee.config  -eval "eunit:test(mgee_unittest_all, [verbose])" -s mgee start



