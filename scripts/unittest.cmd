
cd ..\ebin
del *.beam

cd ..
erl -make

werl -pa ./ebin -eval "eunit:test(mgee_unittest_all, [verbose])"

