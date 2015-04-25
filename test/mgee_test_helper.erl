%%%----------------------------------------------------------------------
%%% File    : mgee_test_helper.erl
%%% Author  : Qingliang
%%% Created : 2010-1-18
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_test_helper).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([create_account/1]).

%%
%% API Functions
%%

create_account(Number) when is_integer(Number) andalso Number >= 1 ->
	lists:foreach(
		fun(Num) -> 
			AccountName = lists:concat([test_, Num]),
			mgee_account_server:create_account(AccountName)
		end, lists:seq(1, Number)).

%%
%% Local Functions
%%

