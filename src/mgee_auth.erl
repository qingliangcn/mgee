%%%----------------------------------------------------------------------
%%% @File    : mgee_auth.erl
%%% @Author  : Qingliang
%%% @Created : 2010-01-02
%%% @Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_auth).

%%
%% Include files
%%
-include("mgee.hrl").
-include("global_lang.hrl").
-include("game_pb.hrl").
%%
%% Exported Functions
%%
-export([auth_user/1]).

-define(AUTH_TIME_OUT, 15000).

%%
%% API Functions
%%

%% @return {passed, AccountName} |
%%         {failed, Reason}      |
%%         {error, Reason}       |
auth_user(ClientSock) ->
	case mgee_packet:recv(ClientSock) of
		{ok, {<<"login">>, Method, Data}} when Method =:= <<"flash_login">> ->
			AuthData = game_pb:decode_m_login_flash_login_tos(Data),
			?TEST_MSG("auth data ~p", [AuthData]), 
			AccountName = AuthData#m_login_flash_login_tos.account_name,
			AccountPwd = AuthData#m_login_flash_login_tos.account_pwd,
			Rtn = auth_by_user_pwd(AccountName, AccountPwd),
			?TEST_MSG("auth result ~p", [Rtn]),
			case Rtn of
				{ok, passed} -> {passed, Method, AccountName};
				{error, wrong_pwd} -> {failed, Method, ?_LANG_LOGIN_WRONG_PWD};
				{error, not_exists} -> {failed, Method, ?_LANG_ACCOUNT_NOT_EXISTS};
				{error, account_empty} -> {failed, Method, ?_LANG_ACCOUNT_EMPTY};
				Other -> 
					?ERROR_MSG("unexpected mgee_account_server:auth_account return info ~p", [Other])
			end;
		{ok, {<<"login">>, Method, _Data}} when Method =:= <<"php_login">> ->
			{error, php_login_not_implemented};
		{ok, Packet} -> ?ERROR_MSG("wrong auth packet ~p ", [Packet]),
						{error, wrong_packet};
		{error, Reason} -> {error, Reason};
		Other -> ?ERROR_MSG("unexpected info ~p", [Other])
	end.

%%
%% Local Functions
%%
auth_by_user_pwd(AccountName, _AccountPwd) ->
	%% http:request()
	?TEST_MSG("account name ~p", [AccountName]),
	case empty(AccountName) of 
		true ->
		   {error, account_empty};
		false ->
			{ok, passed}
	end.

%% auth_by_phpticket(_AccountName, _Ticket) ->
%% 	%% md5
%% 	{ok, passed}.
empty(V) when is_list(V) ->
	erlang:length(V) =:= 0;
empty(V) when is_binary(V) ->
	erlang:byte_size(V) =:= 0.