%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu
%%% @date 2010-1-11
%%% @doc Unit Test for amf3 library
%%% @end
%%%----------------------------------------------------------------------


-module(mgee_unittest_amf).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test() -> ok.

amf3_encode_test() ->
	?assertEqual( amf3:encode({obj, [{x,1}]}) ,
				   <<10,11,1,3,120,4,1,1>> ),
	?assertEqual( amf3:encode({obj, [{x,1},{y,2}]}) ,
				   <<10,11,1,3,120,4,1,3,121,4,2,1>> ),
	
	ok.

amf3_decode_test() ->
	?assertEqual( amf3:decode(  <<10,11,1,3,120,4,1,1>>  )  ,
				   {ok,{obj,[{x,1}]},<<>>} ),
	?assertEqual( amf3:decode(  <<10,11,1,3,120,4,1,3,121,4,2,1>>  )  ,
				   {ok,{obj, [{x,1},{y,2}]}, <<>>} ),
				  
	ok.


amf3_get_obj_data() ->
	Uid=1,
	Data={x,100,y,100},
  	Obj = {obj,[
			{userid, Uid},
			{p, Data}
			]},
  	Obj
  	.

amf3_get_amf3_data() ->
	ok.