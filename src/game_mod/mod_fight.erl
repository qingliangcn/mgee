%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author qingliangcn, 2010-2-3
%%% @doc  fight module
%%% @end
%%%----------------------------------------------------------------------
-module(mod_fight).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
-include("global_lang.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([
		 start/2,
		 handle/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mod_fight_state, {parent}).

%% ====================================================================
%% External functions
%% ====================================================================

handle({ClientSock, Module, Method, Data, _AccountName, Roleid, _RoleName}) ->
	VwId = mgee_virtual_world_router:get_vwid_by_roleid(Roleid),
	?TEST_MSG("~p, ~p", [VwId, Roleid]),
	FightPName = mgee_tool:list_to_atom2(lists:concat([mod_fight_, VwId])),
	?TEST_MSG("FightPName ~p", [FightPName]),
	case Method of
		<<"attack">> -> 
			gen_server:cast(FightPName, 
						{attack, ClientSock, Module, Method, Roleid, Data#m_fight_attack_tos.dest_roleid, Data#m_fight_attack_tos.skillid});
		Other -> 
			?TEST_MSG("unknow method ~p", [Other])
	end,
	ok.


%% ====================================================================
%% Server functions
%% ====================================================================

start(VwId, ParentPid) ->
	Name = mgee_tool:list_to_atom2(lists:concat([mod_fight_, VwId])),
	gen_server:start({local, Name}, ?MODULE, [ParentPid], [{spawn_opt, [{min_heap_size, 256}]}]).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([ParentPid]) ->
	?INFO_MSG("~p init: ~p",[?MODULE, [ParentPid] ]),
    {ok, #mod_fight_state{parent=ParentPid}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({attack, ClientSock, Module, Method, AttackRoleid, DefendRoleid, SkillId}, State) ->
	?TEST_MSG("role ~p wanna to attack role ~p with skill ~p", [AttackRoleid, DefendRoleid, SkillId]),
	case get_role_fight_info(AttackRoleid) of
		{ok, AInfo} ->
			case get_role_fight_info(DefendRoleid) of
				{ok, DInfo} ->
					?TEST_MSG("AInfo ~p", [AInfo]),
					?TEST_MSG("DInfo ~p", [DInfo]),
					case do_fight(AInfo, DInfo, SkillId) of
						{error, DataSelf} ->
							mgee_packet:packet_encode_send2(ClientSock, Module, Method, DataSelf);
						{ok, {DataSelf, DataOther}} ->
							mgee_packet:packet_encode_send2(ClientSock, Module, Method, DataSelf),
							case mgee_misc:get_socket_by_roleid(DInfo#p_role_attr.roleid) of
								{ok, ClientSockDefend} ->
									mgee_packet:packet_encode_send2(ClientSockDefend, Module, Method, DataOther);
								{error, Reason} ->
									?TEST_MSG("cann't find the defend's socket:~p, info ~p", [Reason, DInfo])
							end,
							DataOtherBin = mgee_packet:packet_encode(Module, Method, DataOther),
							gen_server:cast(
								State#mod_fight_state.parent, 
								{broadcast_in_sence, [AttackRoleid, DefendRoleid], DataOtherBin})
					end;
				{error, Reason} ->
					?TEST_MSG("get_role_fight_info of ~p failed:~p", [DefendRoleid, Reason])
			end;
		{error, Reason} ->
			?TEST_MSG("get_role_fight_info of ~p failed:~p", [AttackRoleid, Reason])
	end,
	{noreply, State};
handle_cast(Msg, State) ->
	?TEST_MSG("unexpected cast msg:~p", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?TEST_MSG("unexpected info msg:~p", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?TEST_MSG("mod fight teminate :~p, ~p", [Reason, State]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% a role's attr info should be given when the role enter the vw.  or 
%% when a role change his status to kill , we get his attr info
get_role_fight_info(Roleid)->
	case get(Roleid) of
		undefined ->
			RoleProcessName = mgee_misc:role_process_name(Roleid),
			case mgee_misc:whereis_name({local, RoleProcessName}) of
				undefined ->
					{error, role_not_online};
				Pid ->
					case erlang:is_process_alive(Pid) of
						true ->
							RoleAttr = mgee_role:get_role_attr(Roleid),
							put(Roleid, RoleAttr),
							{ok, RoleAttr};
						false ->
							{error, role_not_online}
					end
			end;
		RoleAttr ->
			{ok, RoleAttr}
	end.

do_fight(AInfo, DInfo, SkillId) ->
	%% need the distance of tuple
	case mod_skill:get_skill_info(SkillId) of
		{ok, SkillInfo} ->
			case get_distance(AInfo#p_role_attr.roleid, DInfo#p_role_attr.roleid) of 
				{ok, Distance} ->
					case SkillInfo#p_skill.attack_area < Distance of
						true ->
							SelfRecord = #m_fight_attack_toc{
											succ=false, 
											reason=?_LANG_FIGHT_NOT_IN_ATTACK_RANGE,
											dest_roleid=DInfo#p_role_attr.roleid,
											skillid=SkillId
											},
							{error, SelfRecord};
						false ->
							SrcEffect = [#p_fight_effect{effect_type=1, effect_value=10}],
							DestEffect = [#p_fight_effect{effect_type=2, effect_value=200}],
							SelfRecord = #m_fight_attack_toc{
											src_effect=SrcEffect,
											dest_effect=DestEffect,
											dest_roleid=DInfo#p_role_attr.roleid,
											skillid=SkillId
											},
							OtherRecord = #m_fight_attack_toc{
												return_self = false,
												src_effect=SrcEffect,
												dest_effect=DestEffect,
												dest_roleid=DInfo#p_role_attr.roleid,
												src_roleid=AInfo#p_role_attr.roleid,
												skillid=SkillId
											},
							{ok, {SelfRecord, OtherRecord}}
					end;
				{error, _Reason} ->
					SelfRecord = #m_fight_attack_toc{
										succ=false, 
										reason=?_LANG_FIGHT_NOT_IN_ATTACK_RANGE,
										dest_roleid=DInfo#p_role_attr.roleid,
										skillid=SkillId
									},
					{error, SelfRecord}
			end;
		{error, _Reason} ->
			SelfRecord = #m_fight_attack_toc{
								succ=false, 
								reason=?_LANG_FIGHT_NOT_IN_ATTACK_RANGE,
								dest_roleid=DInfo#p_role_attr.roleid,
								skillid=SkillId
							},
			{error, SelfRecord}
	end.

get_distance(RoleA, RoleD) ->
	case get_x_y_from_vw_router(RoleA) of
		{ok, TxA, TyA} ->
			case get_x_y_from_vw_router(RoleD) of
				{ok, TxD, TyD} ->
					Distence = math:sqrt(math:pow(TxA - TxD, 2) + math:pow(TyA - TyD, 2)),
					{ok, Distence};
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

get_x_y_from_vw_router(RoleId) ->
	case mgee_virtual_world_router:get_role_state_by_roleid(RoleId) of
		{ok, RoleState} ->
			Tx = mgee_virtual_world_router:get_tx_by_state(RoleState),
			Ty = mgee_virtual_world_router:get_ty_by_state(RoleState),
			{ok, Tx, Ty};
		{error, _} ->
			{error, not_online}
	end.