%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu
%%% @date 2010-2-21
%%% @doc Unit Test for mgee_packet
%%% @end
%%%----------------------------------------------------------------------
	
-module(mgee_unittest_packet).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test() -> ok.

packet_encode_test() ->
	Module = <<"role">>,
	Method = <<"add">>,
	?assertEqual(encode_m_role_add_toc, mgee_packet:get_encode_func(Module, Method) ),
	ok
	.

packet_decode_test() ->
	Module = <<"role">>,
	Method = <<"add">>,
	?assertEqual(decode_m_role_add_tos, mgee_packet:get_decode_func(Module, Method) ),
	

	lists:foreach(
		fun(_T) -> 
			mgee_packet:get_decode_func(Module, Method)
		end, lists:seq(1,10000,1) ),
	ok
	.


