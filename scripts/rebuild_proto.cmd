
cd ../ebin

erlc ../src/proto/protobuffs.erl
erlc ../src/proto/protobuffs_compile.erl
erlc ../src/proto/protobuffs_parser.erl
erlc +debug_info ../src/proto/pokemon_pb.erl

erl -noshell -s protobuffs_compile scan_file "../proto/game.proto" -s erlang halt
move /y game_pb.hrl ../include/

pause
