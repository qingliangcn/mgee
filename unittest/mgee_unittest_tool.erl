%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu
%%% @date 2010-1-11
%%% @doc Unit Test for mgee_tool.erl
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_unittest_tool).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test() -> ok.

sort_test() ->
	L = [ 3,1,65,6,1,3,361,7,9,4],
	L2 = [1,1,3,3,4,6,7,9,65,361],
	SL = mgee_tool:sort(L),
	?assertEqual( SL, L2 ),
	ok.

f2s_test() ->
	?assertEqual("2.00", mgee_tool:f2s(2)),
	?assertEqual("2.01", mgee_tool:f2s(2.00999999999999980000e+000)),
	?assertEqual("0.00", mgee_tool:f2s(0.00000000000000000000e+000)),
	?assertEqual("2.01", mgee_tool:f2s(2.01020000000000020000e+000)),
	?assertError(function_clause, mgee_tool:f2s('2.00')),
	ok.

to_atom_test() ->
	?assertEqual( test,  mgee_tool:to_atom( test ) ),

				  
	ok.

to_list_test() ->
	?assertEqual( [1,2,3] , mgee_tool:to_list([1,2,3]) ),
	?assertEqual( "abcd", mgee_tool:to_list(abcd)  ),
	?assertEqual( "abcd", mgee_tool:to_list( <<"abcd">>) ),
	?assertEqual( "1234", mgee_tool:to_list( <<"1234">>) ),
	?assertEqual( "1234",  mgee_tool:to_list( 1234 ) ),
	?assertEqual( "abcd" , mgee_tool:to_list( "abcd" ) ),
	?assertEqual( [97,98,99,100] , mgee_tool:to_list( "abcd" ) ),

	?assertEqual( "1234.56",  mgee_tool:to_list( 1.23455999999999990000e+003 ) ),
	?assertEqual( "1234.57",  mgee_tool:to_list( 1.23456780000000000000e+003 ) ),
	?assertEqual( "1234.56",  mgee_tool:to_list( 1.23456490000000010000e+003 ) ),

	?assertThrow( other_value , mgee_tool:to_list(  {test, other} )  ),
	ok.

to_binary_test() ->
	?assertEqual( <<"abcd">>, mgee_tool:to_binary( <<"abcd">>) ),
	?assertEqual( <<"abcd">>, mgee_tool:to_binary(abcd)  ),
	?assertEqual( <<"abcd">>, mgee_tool:to_binary("abcd")  ),
	?assertEqual( <<"abcd">>, mgee_tool:to_binary(["abcd"])  ),
	?assertEqual( <<"1234">>,  mgee_tool:to_binary( 1234 ) ),
	?assertEqual( <<"1234.56">>,  mgee_tool:to_binary( 1.23455999999999990000e+003 ) ),
	?assertEqual( <<"1234.57">>,  mgee_tool:to_binary( 1.23456780000000000000e+003 ) ),
	?assertEqual( <<"1234.56">>,  mgee_tool:to_binary( 1.23456490000000010000e+003 ) ),

	?assertThrow( other_value , mgee_tool:to_list( {test, other} )  ),
	ok.

to_integer_test() ->
	?assertEqual( 123,   mgee_tool:to_integer( <<"123">>) ),
	?assertEqual( 12345, mgee_tool:to_integer("12345")  ),
	?assertEqual( 1234,  mgee_tool:to_integer( 1234 ) ),
	?assertEqual( 1235,  mgee_tool:to_integer( 1.23455999999999990000e+003 ) ),
	?assertEqual( 1234,  mgee_tool:to_integer( 1.23449000000000000000e+003 ) ),
	
	?assertError( badarg,  mgee_tool:to_integer(["1234"])  ),
	?assertThrow( other_value , mgee_tool:to_integer( {test, other} )  ),
	ok.
	

to_tuple_test() ->
	?assertEqual( {abc},  mgee_tool:to_tuple( {abc} ) ),
	?assertEqual( {abc},  mgee_tool:to_tuple( abc ) ),
	ok.

	