
cd ..\ebin
del *.beam

cd ..
erl -make

cd ebin
erl -noshell -s protobuffs_compile scan_file "../proto/game.proto" -s erlang halt
move /y game_pb.hrl ../include/

pause

