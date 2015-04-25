
cd ../ebin

erlc ../src/library/amf3.erl

erlc ../src/library/memcached.erl

erlc -I ../include/  ../src/library/mysql.erl
erlc -I ../include/  ../src/library/mysql_auth.erl
erlc -I ../include/  ../src/library/mysql_conn.erl
erlc -I ../include/  ../src/library/mysql_recv.erl

