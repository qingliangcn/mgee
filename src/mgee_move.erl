%%%----------------------------------------------------------------------
%%% File    : mgee_walk.erl
%%% Author  : Qingliang
%%% Created : 2010-1-6
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_move).

%%
%% Include files
%%
-include("mgee.hrl").
-include("game_pb.hrl").
%%
%% Exported Functions
%%
-export([handle/1]).

-define(DEV, 1).

-ifdef(DEV).

-define(FUNC_POS_SYNC(RoleId, DataRecord), pos_sync(RoleId, DataRecord)).

-else.

-define(FUNC_POS_SYNC(RoleId, DataRecord), ok.

-endif.

%%
%% API Functions
%%

handle({ClientSock, Module, Method, DataRecord, _AccountName, RoleId, _RoleName}) ->
	case Method of
		<<"walk">> -> 
			walk(ClientSock, Module, Method, DataRecord, RoleId);
		<<"walk_path">> -> 
			walk_path(ClientSock, Module, Method, DataRecord, RoleId);
		<<"run">> -> 
			run(RoleId, DataRecord);
		<<"fly">> -> 
			fly(RoleId, DataRecord);
		<<"sit">> -> 
			sit(RoleId, DataRecord);
		<<"follow">> -> 
			follow(RoleId, DataRecord);
		<<"pos_sync">> ->
			?FUNC_POS_SYNC(RoleId, DataRecord);
		_ ->
			  ?TEST_MSG("unexpected method ~p", [Method])
	end,
	ok.

%%
%% Local Functions
%%

walk(ClientSock, Module, Method, DataRecord, RoleId) ->
	case mgee_virtual_world_router:get_role_state_by_roleid(RoleId) of
		{ok, RoleState} ->
			VWID = mgee_virtual_world_router:get_vwid_by_state(RoleState),
			VName = mgee_virtual_world_router:get_virtual_world_name(VWID),
			gen_server:cast(VName, {walk, ClientSock, Module, Method, DataRecord, RoleId});
		{error, not_found} ->
			?DEBUG("role ~p not in vw", [RoleId])
	end.

walk_path(ClientSock, Module, Method, DataRecord, RoleId)
  when is_record(DataRecord, m_move_walk_path_tos) ->
	?TEST_MSG("enter mgee_move", []),
	case mgee_virtual_world_router:get_role_state_by_roleid(RoleId) of
		{ok, RoleState} ->
			VWID = mgee_virtual_world_router:get_vwid_by_state(RoleState),
			VName = mgee_virtual_world_router:get_virtual_world_name(VWID),
			gen_server:cast(VName, {walk_path, ClientSock, Module, Method, DataRecord, RoleId});
		{error, not_found} ->
			?DEBUG("role ~p not in vw", [RoleId])
	end.

pos_sync(RoleId, DataRecord) ->
	Tx = DataRecord#m_move_pos_sync_tos.tx,
	Ty = DataRecord#m_move_pos_sync_tos.ty,
	Pid = mgee_misc:get_role_pid(RoleId),
	gen_server:cast(mgee_virtual_world_router, {pos_sync, Pid, Tx, Ty}),
	case mgee_virtual_world_router:get_role_state_by_roleid(RoleId) of
		{ok, RoleState} ->
			VWID = mgee_virtual_world_router:get_vwid_by_state(RoleState),
			VName = mgee_virtual_world_router:get_virtual_world_name(VWID),
			gen_server:cast(VName, {pos_sync, RoleId, Tx, Ty});
		{error, not_found} ->
			?DEBUG("role ~p not in vw", [RoleId])
	end.

%% data is like this {id:123,x:12,y:34}
run(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"run">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {run, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
fly(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"fly">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {fly, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
sit(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"sit">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {sit, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
%% follow the team leader's walk.
follow(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"follow">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {follow, DataSend}
				end,
				Clients),
	ok.

