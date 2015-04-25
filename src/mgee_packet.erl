%% Author: Administrator
%% Created: 2010-1-3
%% Description: TODO: Add description to mgee_packet_decode
-module(mgee_packet).

%%
%% Include files
%%
-include("mgee.hrl").
%%
%% Exported Functions
%%
-export([
		 recv/1, 
		 send/2, 
		 recv/2, 
		 packet_encode_send/4,
		 packet_encode_send2/4,
		 packet_encode/3
  ]).

-export([
		 packet/3, 
		 unpack/1,
		 get_decode_func/2,
		 get_encode_func/2,
		 encode/3,
		 decode/3
		]).

%% here we don't care the cross domain file
recv(ClientSock) ->
	case gen_tcp:recv(ClientSock, 2) of
		{ok, PacketLenBin} -> 
			<<PacketLen:16>> = PacketLenBin,
			case gen_tcp:recv(ClientSock, PacketLen) of
				{ok, <<"00">>} ->
					{ok, heartbeat};
				{ok, RealData} ->
					?TEST_MSG("recv data ~p", [RealData]),
					{ok, mgee_packet:unpack(RealData)};
				
				{error, Reason} ->
					?ERROR_MSG("read packet data failed with reason: ~p on socket ~p", [Reason, ClientSock]),
					{error, Reason}
				end;
		
		{error, Reason} -> 
			?ERROR_MSG("read packet length failed with reason: ~p on socket ~p", [Reason, ClientSock]),
			{error, Reason}
	end.

%% @desc sometime we need the Timeout option
recv(ClientSock, Timeout) ->
	case gen_tcp:recv(ClientSock, 2, Timeout) of
		{ok, PacketLenBin} -> 
			<<PacketLen:16>> = PacketLenBin,
			case gen_tcp:recv(ClientSock, PacketLen, Timeout) of
				{ok, RealData} ->
					?TEST_MSG("recv data ~p", [RealData]),
					{ok, mgee_packet:unpack(RealData)};
				{error, Reason} ->
					?ERROR_MSG("read packet data failed with reason: ~p on socket ~p", [Reason, ClientSock]),
					{error, Reason}
			end;
		{error, Reason} -> 
			?ERROR_MSG("read packet length failed with reason: ~p on socket ~p", [Reason, ClientSock]),
			{error, Reason}
	end.

packet_encode_send2(ClientSock, Module, Method, DataRecord) ->
	case (catch packet_encode_send(ClientSock, Module, Method, DataRecord) ) of
		{'EXIT', Info} -> 
			?ERROR_MSG("error when router packet_encode_send Module:~p, Method:~p, Info:~p", 
 						[Module, Method, Info]);
		_ -> ok
	end.									

packet_encode_send(ClientSock, Module, Method, DataRecord) ->
	DataBin = encode(Module, Method, DataRecord),
	send(ClientSock, mgee_packet:packet(Module, Method, DataBin)).

send(ClientSock, Bin) ->
	?TEST_MSG("packet send ~p ", [Bin]),
	PacketLen = erlang:byte_size(Bin),
	SendBin = <<PacketLen:16, Bin/binary>>,
	case gen_tcp:send(ClientSock, SendBin) of
		ok -> ?TEST_MSG("!!!packet send ~p ok ", [SendBin]),ok;
		{error, closed} -> {error, closed};
		{error, Reason} -> {error, Reason}
	end.

packet_encode(Module, Method, DataRecord) ->
	mgee_packet:packet(Module, Method, encode(Module, Method, DataRecord)).

packet(Module, Method, Data) when is_list(Module) and is_list(Method) ->
	Module2 = list_to_binary(Module),
	Method2 = list_to_binary(Method),
	packet2(Module2, Method2, Data);
packet(Module, Method, Data) when is_list(Module) ->
	Module2 = list_to_binary(Module),
	packet2(Module2, Method, Data);
packet(Module, Method, Data) when is_list(Method) ->
	Method2 = list_to_binary(Method),
	packet2(Module, Method2, Data);
packet(Module, Method, Data) when is_binary(Module) and is_binary(Method) ->
	packet2(Module, Method, Data).

packet2(Module, Method, Data) ->
	ModuleLen = erlang:length(binary_to_list(Module)),
	MethodLen = erlang:length(binary_to_list(Method)),
	if erlang:byte_size(Data) >= 100 ->
		DataCompress = zlib:compress(Data),
		<<1:1, ModuleLen:7, MethodLen:8, Module/binary, Method/binary, DataCompress/binary>>;
	true ->
		<<ModuleLen:8, MethodLen:8, Module/binary, Method/binary, Data/binary>>
	end.

unpack(DataRaw) ->
	<<IsZip:1, ModuleLen:7, MethodLen:8, Bin/binary>> = DataRaw,
	<<Module:ModuleLen/binary, Method:MethodLen/binary, Data/binary>> = Bin,
	case IsZip of
		0 -> {Module, Method, Data};
		1 -> 
			case Data of
				<<>> -> {Module, Method, <<>>};		% some method, may be not protobuf data.
				_    ->	{Module, Method, zlib:uncompress(Data)}
			end
	end.

get_decode_func(Module, Method) ->
	mgee_tool:list_to_atom2(
		lists:concat(
			[decode_m_, mgee_tool:to_list(Module), "_",mgee_tool:to_list(Method), "_tos"])).

get_encode_func(Module, Method) ->
  	mgee_tool:list_to_atom2(
		lists:concat(
			[encode_m_, mgee_tool:to_list(Module), "_",mgee_tool:to_list(Method), "_toc"])).

encode(Module, Method, DataRecord) ->
	apply(game_pb, get_encode_func(Module, Method), [DataRecord]).

decode(Module, Method, DataBin) ->
	apply(game_pb, get_decode_func(Module, Method), [DataBin]).
