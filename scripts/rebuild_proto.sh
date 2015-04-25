#!/bin/bash

erlc -o ../ebin/ ../src/proto/protobuffs.erl
erlc -o ../ebin/ ../src/proto/protobuffs_compile.erl
erlc -o ../ebin/ ../src/proto/protobuffs_parser.erl
erlc +debug_info -o ../ebin/ ../src/proto/pokemon_pb.erl

erl -noshell -s protobuffs_compile scan_file "../proto/game.proto" -pa ../ebin/ -s erlang halt

/bin/mv -f game_pb.hrl ../include/
/bin/mv -f game_pb.beam ../ebin/

echo "编译proto对应的hrl和beam文件成功"
