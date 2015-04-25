%%%----------------------------------------------------------------------
%%% File    : mgee_test_chat.erl
%%% Author  : Qingliang
%%% Created : 2010-1-28
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_test_chat).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([
		 start_new/3
		 ]).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

start_new(N, Host, Port) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
		{ok, ConnSocket} ->
			handshaking(N, ConnSocket);
		{error, Reason} ->
			?DEBUG("connect ~p:~p failed: ~p", [Host, Port, Reason])
	end.

handshaking(N, ConnSocket) ->
	timer:sleep(50),
	case gen_tcp:send(ConnSocket, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>) of
		ok ->
			login(N, ConnSocket);
		{error, Reason} ->
			?DEBUG("send handshacking failed with reason ~p", [Reason])
	end.

login(N, ConnSocket) ->
	timer:sleep(50),
	Module = <<"login">>,
	Method = <<"flash_login">>,
	AccountName = lists:concat(["test_chat_", N]),
	DataRecord = #m_login_flash_login_tos{account_name=AccountName, account_pwd="123456"},
	?DEBUG("~p", [DataRecord]),
	DataBin = game_pb:encode_m_login_flash_login_tos(DataRecord),
	?DEBUG("~p", [DataBin]),
	case mgee_packet:send(ConnSocket, mgee_packet:packet(Module, Method, DataBin)) of
		ok ->
			case mgee_packet:recv(ConnSocket) of
				{ok, Rtn} ->
					{Module, Method, RtnBin} = Rtn,
					?DEBUG("~p", [RtnBin]),
					RtnRecord = game_pb:decode_m_login_flash_login_toc(RtnBin),
					?DEBUG("~p", [RtnRecord]),
					case RtnRecord#m_login_flash_login_toc.succ of
						true ->
							?DEBUG("login successful", []),
							Rolelist = (RtnRecord#m_login_flash_login_toc.result)#p_role_list.role,
							?DEBUG("~p", [Rolelist]),
							if 
								length(Rolelist) > 0 ->
									enter(N, Rolelist, ConnSocket);
								true -> 
									create_role(N, ConnSocket)
							end;
						false ->
							?DEBUG("login failed：~p", [RtnRecord#m_login_flash_login_toc.reason])
					end;
				{error, Reason} ->
					?DEBUG("recv login result error : ~p", [Reason])
			end;
		{error, Reason} ->
			?DEBUG("send login data error : ~p", [Reason])
	end.

create_role(N, ConnSocket) ->
	timer:sleep(50),
	Module = <<"role">>,
	Method = <<"add">>,
	Rolename = lists:concat([rolename_test_, N]),
	DataRecord = #m_role_add_tos{rolename=Rolename,sex = mgee_tool:random(1,2) - 1, skinid = mgee_tool:random(1,8)},
	DataBin = game_pb:encode_m_role_add_tos(DataRecord),
	case mgee_packet:send(ConnSocket, mgee_packet:packet(Module, Method, DataBin)) of
		ok ->
			case mgee_packet:recv(ConnSocket) of
				{ok, Rtn} ->
					{Module, Method, RtnBin} = Rtn,
					RtnRecord = game_pb:decode_m_role_add_toc(RtnBin),
					case RtnRecord#m_role_add_toc.succ of
						true ->
							?DEBUG("add role succ", [RtnRecord]),
							Rolelist = (RtnRecord#m_role_add_toc.result)#p_role_list.role,
							enter(N, Rolelist, ConnSocket);
						false ->
							?DEBUG("add role failed:~p", [RtnRecord#m_role_add_toc.reason]),
							timer:sleep(3000),
							create_role(N, ConnSocket)
					end;
				{error, Reason} ->
					?DEBUG("send login data error : ~p", [Reason])
			end;
		{error, Reason} ->
			?DEBUG("send login data error : ~p", [Reason])
	end.

enter(N, RoleList, ConnSocket) ->
	timer:sleep(50),
	Module = <<"role">>,
	Method = <<"enter">>,
	?DEBUG("~p", RoleList),
	Role = lists:last(RoleList),
	Roleid = Role#p_game_role.roleid,
	VwId = Role#p_game_role.vwid,
	DataRecord = #m_role_enter_tos{roleid=Roleid},
	DataBin = game_pb:encode_m_role_enter_tos(DataRecord),
	case mgee_packet:send(ConnSocket, mgee_packet:packet(Module, Method, DataBin)) of
		ok ->
			case mgee_packet:recv(ConnSocket) of
				{ok, Rtn} -> 
					{Module, Method, RtnBin} = Rtn,
					RtnRecord = game_pb:decode_m_role_enter_toc(RtnBin),
					case RtnRecord#m_role_enter_toc.succ of
						true ->
							?DEBUG("etner role succ", []),
							%%chat_circle(N, ConnSocket);
							enter_vw(N, ConnSocket, VwId);
						false ->
							?DEBUG("enter role failed : ~p", [RtnRecord#m_role_enter_toc.reason])
					end;
				{error, Reason}->
				  	?DEBUG("send login data error : ~p", [Reason])
			end;
		{error, Reason} ->
			?DEBUG("send login data error : ~p", [Reason])
	end.

enter_vw(N, ConnSocket, VwId) ->
	timer:sleep(50),
	Module = <<"vw">>,
	Method = <<"enter">>,
	DataRecord = #m_vw_enter_tos{vwid=VwId},
	DataBin = game_pb:encode_m_vw_enter_tos(DataRecord),
	case mgee_packet:send(ConnSocket, mgee_packet:packet(Module, Method, DataBin)) of
		ok ->
			case mgee_packet:recv(ConnSocket) of
				{ok, Rtn} -> 
					{Module, Method, RtnBin} = Rtn,
					RtnRecord = game_pb:decode_m_vw_enter_toc(RtnBin),
					case RtnRecord#m_vw_enter_toc.succ of
						true ->
							?DEBUG("enter vw succ", []),
							chat_circle(N, ConnSocket);
						false ->
							?DEBUG("enter vw failed:~p", [RtnRecord#m_vw_enter_toc.reason])
					end;
				{error, Reason}->
				  	?DEBUG("send login data error : ~p", [Reason])
			end;
		{error, Reason} ->
			?DEBUG("send login data error : ~p", [Reason])
	end.

chat_circle(N, ConnSocket) ->
	?DEBUG("start chat", []),
	timer:sleep(5000),
	?DEBUG("start recv", []),
	spawn(fun() -> chat_recv(ConnSocket) end),
	chat_send(N, ConnSocket).

chat_recv(ConnSocket) ->
	mgee_packet:recv(ConnSocket),
	chat_recv(ConnSocket).

chat_send(N, ConnSocket) ->
	Module = <<"chat">>,
	Method = <<"world">>,
	DataRecord = #m_chat_world_tos{body = lists:concat(["我是角色：", N])},
	DataBin = game_pb:encode_m_chat_world_tos(DataRecord),
	mgee_packet:send(ConnSocket, mgee_packet:packet(Module, Method, DataBin)),
	timer:sleep(3000),
	chat_send(N, ConnSocket).