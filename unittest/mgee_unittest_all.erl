%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu
%%% @date 2010-1-11
%%% @doc Unit Test 
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_unittest_all).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test() -> ok.

all_test_() -> [mgee_unittest_amf,
                %mgee_unittest_memcache,
                %mgee_unittest_mysql,
                %mgee_unittest_chat,
				mgee_unittest_tool,
				mgee_unittest_packet,
                mgee_unittest_router ].
