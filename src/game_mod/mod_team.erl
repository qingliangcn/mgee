%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-19
%%% @doc every team use one mod_team gen_server to provide role data and team data,
%%%		when the first person join the team, create one mod_team gen_server.
%%% @end
%%%----------------------------------------------------------------------

-module(mod_team).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("mgee.hrl").
-include("game_pb.hrl").
-include("global_lang.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record( mod_team_status, {
					 teamid,
					 teamdesc,
					 create_time, 
					 follow_count,
					 team_role_list , 
					 pid_list, 
					 list_invite
						  } ).

%% ====================================================================
%% External functions
%% ====================================================================


start_link({Teamid, TeamDesc, Roleid, RoleName}) ->
	gen_server:start_link(?MODULE, {Teamid, TeamDesc, Roleid, RoleName}, []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init({Teamid, TeamDesc, Roleid, RoleName}) ->
	?INFO_MSG("~p init: ~p",[?MODULE, {Teamid, TeamDesc, Roleid, RoleName}]),
	Now = mgee_timer:now(),
	PidList = [ {Roleid, mgee_misc:get_role_pid(Roleid), Now } ],
	TeamRoleList = [ {Roleid,
					  #p_team_role{roleid= Roleid, 
						 rolename=RoleName, 
						 is_leader=true,
						 team_order=1 
						} } ],
	State = #mod_team_status{teamid = Teamid, 
							 teamdesc = TeamDesc,
							 create_time = Now, 
							 follow_count = 0, 	% how many member follow the leader's walk.
							 team_role_list=TeamRoleList, 	% [{roleid, p_team_role}, {roleid, p_team_role},...]
							 pid_list = PidList,	% [{roleid, pid, time},{roleid, pid, time},{roleid, pid, time},....]
							 list_invite = []		% [{roleid, time},{roleid, time},{roleid, time},....]
							},
    {ok,  State}.

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


% @doc handle list role event call
handle_call( list , _From, State) ->
	#mod_team_status{team_role_list = TeamRoleList} = State,
	PRoleList = get_p_team_role_list( TeamRoleList ),
	Reply = #p_team_role_list{ role = PRoleList },
	?TEST_MSG("mod_team handle_call list: ~p", [Reply]),
    {reply, Reply, State};

handle_call( can_be_invited, _From, State) ->
	?TEST_MSG("~p can_be_invited,",[self()]),
	#mod_team_status{team_role_list = TeamRoleList} = State,
	Reply = if length(TeamRoleList) =< 1 ->	true;
									true -> false
			end,	
	{reply, Reply, State};

% @doc when a role join to a new team, it must exit old team
handle_call( {exit, join_to_new_team, NewTeamid}, _From, State) ->
	#mod_team_status{teamid = Teamid, team_role_list = TeamRoleList} = State,
	?TEST_MSG("~p exit, join_to_other_team:~p, current teamid ~p, current team role list:~p",[self(), NewTeamid, Teamid, TeamRoleList]),
	if length(TeamRoleList) =< 1 andalso Teamid =/= NewTeamid ->
		?DEBUG("~p teamid ~p exit succ.", [self(), Teamid]),
		gen_server:cast(mod_team_server, {delete_team, Teamid}),		
		% because of the supervisor, this must use {stop, normal, Reply, State}
		{stop, normal, true, State};
	true ->
		{reply, false, State}
	end
	;


%% % @doc team member offline...
%% handle_call( {offline, ClientSock, Roleid, RoleName}, _From, State) ->
%% 	%% TODO:.....
%% 	Reply = doing,
%% 	?TEST_MSG("offline team, ~p", [Reply]),
%%     {reply, Reply, State};
%% 
%% 

% @doc handle list role event call
handle_call( {info, Teamid} , _From, State) ->
	?TEST_MSG("mod_team handle_call info: ~p", [Teamid]),
    {reply, State, State};


handle_call(Request, From, State) ->
	?DEBUG("~p handle_cal from ~p : ~p", [?MODULE, From, Request]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


% @doc handle follow team leader event call
handle_cast( {do_follow, ClientSock, Roleid, RoleName, TeamId, SetFollow},  State) ->
	#mod_team_status{pid_list = PidList, team_role_list = TeamRoleList} = State,
%	PRoleList = get_p_team_role_list( TeamRoleList ),

	?TEST_MSG("team do_follow, status: ~p set follow: ~p",[State, SetFollow]),
	Result1 =
	case proplists:lookup(Roleid, TeamRoleList) of
		{Roleid, TeamRole} ->
			if TeamRole#p_team_role.is_leader ->
				   {false, ?_LANG_TEAM_FOLLOW_FAIL_IS_LEADER};
			true ->
				case SetFollow of
					1 ->	% set to follow
						if TeamRole#p_team_role.is_follow =:= true ->
					   		{false, ?_LANG_TEAM_FOLLOW_FAIL_REPEAT};
				   		true ->
							% TODO: calc the distance between leader and self.
							% if the distance too far, cann't follow
							ok
						end;
					2 -> 	% set to not follow
						if TeamRole#p_team_role.is_follow =:= false ->
					   		{false, ?_LANG_TEAM_FOLLOW_FAIL_REPEAT};
				   		true ->
							% can set to not follow, everywhere, everytime.
							ok
						end;
					_ ->	{false, ?_LANG_PARAM_ERROR}
				end
			end;
		_ ->
			{false, ?_LANG_TEAM_NOT_IN}
	end,
	?TEST_MSG("team do_leave, result 1: ~p",[Result1]),
	StateNew = 
	case Result1 of 
		{false, Reason} ->
			SendSelf = #m_team_follow_toc{succ=false, reason = Reason, set_follow=SetFollow },
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"follow">>, SendSelf),
			State;
		ok ->			
			TeamRole1 = proplists:get_value(Roleid, TeamRoleList),
			{NewFollowCount, TeamRole2} =
			case SetFollow of
				1 ->	
						{State#mod_team_status.follow_count + 1,
						 TeamRole1#p_team_role{is_follow = true} };
			    2	 -> {State#mod_team_status.follow_count - 1,
						 TeamRole1#p_team_role{is_follow = false} }
			end,
			TeamRoleList2 = proplists:delete(Roleid, TeamRoleList),			
			TeamRoleList3 = [ {Roleid, TeamRole2} | TeamRoleList2],
			
			% notice mod_team_server to change the ets data.
			%gen_server:cast(mod_team_server, {update_follow_count_at_team, TeamId, NewFollowCount } ),			
			
			SendSelf = #m_team_follow_toc{succ=true, teamid= TeamId,
										  set_follow=SetFollow,  roleid=Roleid, rolename=RoleName  },
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"follow">>, SendSelf),

			SendMember = #m_team_follow_toc{return_self = false, teamid= TeamId,
										  set_follow=SetFollow,  roleid=Roleid, rolename=RoleName  },

			PidList2 = proplists:delete(Roleid, PidList),
			lists:foreach( fun({_, MPid, _}) ->
					case mgee_misc:get_socket_by_rolepid(MPid) of
						{ok, Sock2} ->
							mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"follow">>, SendMember);
						_ ->
							ingore
					end
				end, PidList2),
			
			State#mod_team_status{
							 team_role_list=TeamRoleList3,
							 follow_count = NewFollowCount
							};
		_ ->
			?ERROR_MSG("unknown error",[]),
			State
	end,
	?TEST_MSG("team do_follow, new status: ~p",[StateNew]),
    {noreply, StateNew};

% @doc handle list role event call
handle_cast( {do_list, ClientSock, Roleid, _RoleName},  State) ->
	#mod_team_status{teamid = Teamid, team_role_list = TeamRoleList} = State,
	PRoleList = get_p_team_role_list( TeamRoleList ),
	P = #p_team_role_list{ role = PRoleList },
	SendSelf = #m_team_list_toc{succ=true, list = P, teamid = Teamid},
	?TEST_MSG("~p list, roleid:~p result:~p,",[self(), Roleid, SendSelf]),
	mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"list">>, SendSelf),
    {noreply, State};

handle_cast( {do_invite, ClientSock, Roleid, RoleName, InviteRoleid, InviteTeamPidTuple}, State) ->
	?TEST_MSG("~p handle_cast invite:~p, ~p",[self(), {do_invite, ClientSock, Roleid, RoleName, InviteRoleid, InviteTeamPidTuple}, State]),
	#mod_team_status{teamid = Teamid, list_invite = InviteList, team_role_list = TeamRoleList} = State,
	InvitePid = mgee_misc:get_role_pid(InviteRoleid),	
	Result = 
	if is_pid(InvitePid) ->
		case proplists:is_defined(InviteRoleid, TeamRoleList) of
		true ->
				   % this role current is at my team
				   {false, ?_LANG_TEAM_INVITE_FAIL_REPEAT};
		false ->
			case InviteTeamPidTuple of 
				{ok, InviteRoleid,  _ , Pid2} when is_pid(Pid2) ->
					case gen_server:call(Pid2, can_be_invited ) of
						true ->
							ok;
						false ->
							% this role already to have a team
							{false, ?_LANG_TEAM_INVITE_FAIL_EXIST}
					end;
				_ ->
					% this role not at other team, and not create a team, sure, invite it immediately.
					ok			
			end
		end;
	true ->
			% this role is not online now because can not get role's pid
			{false, ?_LANG_TEAM_INVITE_FAIL_OFFLINE}
	end,
	?TEST_MSG("team do_invite, result 1: ~p",[Result]),
	StateNew = 
	case Result of 
		{false, Reason} ->
			SendSelf = #m_team_invite_toc{succ=false, reason = Reason},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"invite">>, SendSelf),
			State;
		ok ->
			SendSelf = #m_team_invite_toc{succ=true},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"invite">>, SendSelf),
			
			SendInvite = #m_team_invite_toc{return_self=false, 
											roleid=Roleid, rolename=RoleName, teamid=Teamid},			
			case mgee_misc:get_socket_by_rolepid(InvitePid) of
				{ok, Sock2} ->
					mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"invite">>, SendInvite);
				_ ->
					ignore
			end,			
			State#mod_team_status{ list_invite = [{InviteRoleid, mgee_timer:now()} | InviteList] };
		_ ->
			?ERROR_MSG("unknown error",[]),
			State
	end,
	?TEST_MSG("team do_invite, new status: ~p",[StateNew]),
    {noreply, StateNew};

% @doc accept and join in the team
handle_cast( {do_accept, ClientSock, Roleid, RoleName, Teamid, AcceptorTeamPidTuple}, State) ->
	#mod_team_status{pid_list = PidList, list_invite = InviteList, team_role_list = TeamRoleList} = State,
	?TEST_MSG("team do_accept, status: ~p",[State]),
	Result1 =
	case proplists:lookup(Roleid, TeamRoleList) of
		{Roleid, _TeamRole} ->
			{false, ?_LANG_TEAM_ACCEPT_REPEAT};
		_ ->
			if length(TeamRoleList) >= ?TEAM_MAX_ROLE_COUNT ->
				   {false, ?_LANG_TEAM_ACCEPT_FAIL_MAX_LIMIT};
			true ->
				case proplists:lookup(Roleid, InviteList) of
					{Roleid, _InviteTime} ->
						ok;
					_ ->
						% not found invite data
						{false, ?_LANG_TEAM_ACCEPT_FAIL_NOT_INVITE}
				end
			end
	end,
	?TEST_MSG("team do_accept, result 1: ~p",[Result1]),
	Result2 = 
	case Result1 of
		ok ->
			% before join to new team, this role's must delete his own team. 
			case AcceptorTeamPidTuple of 
				{ok, Roleid,  _ , Pid2} when is_pid(Pid2) ->
					case gen_server:call(Pid2, {exit, join_to_new_team, Teamid} ) of
						true ->
							ok;
						false ->
							% this role already to have a team
							{false, ?_LANG_TEAM_ACCEPT_FAIL_EXIT_OLD_TEAM}
					end;
				_ ->
					% this role not at other team, and not create a team, sure..
					ok			
			end;			
		_ ->
			Result1
	end,	
	?TEST_MSG("team do_accept, result 2: ~p",[Result2]),
	StateNew = 
	case Result2 of 
		{false, Reason} ->
			SendSelf = #m_team_accept_toc{succ=false, reason = Reason},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"accept">>, SendSelf),
			State;
		ok ->
			% notice mod_team_server to change the ets data.
			gen_server:cast(mod_team_server, {update_role_team, Roleid, RoleName, Teamid, length(TeamRoleList) + 1 } ),
			
			RolePid = mgee_misc:get_role_pid(Roleid),
			
			% TODO: get role's attribute, and save attr to team_role_list
			%RoleAttr = gen_server:call(RolePid, {get_role_attr, Roleid} ),

			AddTeamRole = #p_team_role{			roleid= Roleid, 
				 								rolename=RoleName,
												% todo: ....
												%level=RoleAttr...
												level=mgee_tool:random(90,99),
				 								team_order=get_next_team_order_num(TeamRoleList),
												is_leader = false
											},
			OldPRoleList = get_p_team_role_list( TeamRoleList ),
			P = #p_team_role_list{ role = [AddTeamRole | OldPRoleList] },

			SendSelf = #m_team_accept_toc{succ=true, list = P, teamid= Teamid},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"accept">>, SendSelf),

			SendMember = #m_team_accept_toc{return_self = false, list = P, 
											roleid = Roleid, rolename = RoleName, teamid = Teamid},
			lists:foreach( fun({_, MPid, _}) ->
					case mgee_misc:get_socket_by_rolepid(MPid) of
						{ok, Sock2} ->
							mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"accept">>, SendMember);
						_ ->
							ingore
					end
				end, PidList),
			
			TimeNow = mgee_timer:now(),
			PidList2 = [ {Roleid, RolePid, TimeNow} | PidList],
			TeamRoleList2 = [ {Roleid,AddTeamRole} | TeamRoleList ],
			InviteList2 = proplists:delete(Roleid, InviteList),
			State#mod_team_status{
							 team_role_list=TeamRoleList2,
							 pid_list = PidList2,
							 list_invite = InviteList2
							};
		_ ->
			?ERROR_MSG("unknown error",[]),
			State
	end,
	?TEST_MSG("team do_accept, new status: ~p",[StateNew]),
    {noreply, StateNew};


% @doc refuse the invite 
handle_cast( {do_refuse, _ClientSock, Roleid, RoleName, TeamId }, State) ->
	#mod_team_status{pid_list = PidList, list_invite = InviteList} = State,
	?TEST_MSG("team do_refuse, status: ~p",[State]),
	StateNew =
	case proplists:lookup(Roleid, InviteList) of
		{Roleid, _InviteTime} ->			
			SendMember = #m_team_refuse_toc{roleid = Roleid, rolename = RoleName, teamid = TeamId},
			lists:foreach( fun({_, MPid, _}) ->
					case mgee_misc:get_socket_by_rolepid(MPid) of
						{ok, Sock2} ->
							mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"refuse">>, SendMember);
						_ ->
							ingore
					end
				end, PidList),			
			
			InviteList2 = proplists:delete(Roleid, InviteList),
			State#mod_team_status{
							 list_invite = InviteList2
							};
		_ ->
			% not found invite data
			%{false, ?_LANG_TEAM_REFUSE_FAIL_NOT_INVITE}
			%ingore
			State
	end,
	?TEST_MSG("team do_refuse, new status: ~p",[StateNew]),
    {noreply, StateNew};

% @doc leave/exit the team 
handle_cast( {do_leave, ClientSock, Roleid, RoleName, TeamId }, State) ->
	#mod_team_status{team_role_list = TeamRoleList} = State,
	?TEST_MSG("team do_leave, status: ~p",[State]),
	Result1 =
	case proplists:lookup(Roleid, TeamRoleList) of
		{Roleid, TeamRole} ->
			if TeamRole#p_team_role.is_leader ->
				do_leave_leader(ClientSock, Roleid, RoleName, State );
			true ->
				do_leave_member(ClientSock, Roleid, RoleName, State )
			end;
		_ ->
			{false, ?_LANG_TEAM_LEAVE_FAIL_NOT_IN}
	end,
	?TEST_MSG("team do_leave, result 1: ~p",[Result1]),
	case Result1 of 
		{false, Reason} ->
			SendSelf = #m_team_leave_toc{succ=false, reason = Reason},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"leave">>, SendSelf),
			{noreply, State};
		{ok, stop} ->
			% disband the team
			?DEBUG("~p teamid ~p stop, because do_leave.", [self(), TeamId]),
			gen_server:cast(mod_team_server, {delete_team, TeamId}),		
			% because of the supervisor, this must use {stop, normal, State}
			{stop, normal, State};
		
		{ok, StateNew} ->
			?TEST_MSG("team do_leave, new status: ~p",[StateNew]),
    		{noreply, StateNew};
		_ ->
			?ERROR_MSG("unknown error",[]),
			{noreply, State}
	end
	;

% @doc the leader kick sameone out
handle_cast( {do_kick, ClientSock, Roleid, _RoleName, KickRoleid }, State) ->
	#mod_team_status{teamid = TeamId, pid_list = PidList, team_role_list = TeamRoleList} = State,
	?TEST_MSG("team do_kick, status: ~p",[State]),
	Result1 =
	case proplists:lookup(Roleid, TeamRoleList) of
		{Roleid, TeamRole} ->
			if TeamRole#p_team_role.is_leader ->
				case proplists:lookup(KickRoleid, TeamRoleList) of
						{KickRoleid, _} ->
							if Roleid =:= KickRoleid ->
									{false, ?_LANG_TEAM_KICK_FAIL_SELF};   
								true ->
									ok
							end;
						_ ->
							{false, ?_LANG_TEAM_KICK_FAIL_NOT_IN}
				end;
			true ->
				% must leader authority 
				{false, ?_LANG_TEAM_LEADER_AUTHORITY}
			end;
		_ ->
			{false, ?_LANG_TEAM_NOT_IN}
	end,
	?TEST_MSG("team do_kick, result 1: ~p",[Result1]),
	StateNew =
	case Result1 of 
		{false, Reason} ->
			SendSelf = #m_team_kick_toc{succ=false, reason = Reason},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"kick">>, SendSelf),
			State;
		ok ->
			% notice mod_team_server to change the ets data.
			% this role succ leave team, then must delete roleid at this teamid.
			gen_server:cast(mod_team_server, {delete_role_at_team, KickRoleid, TeamId, length(TeamRoleList) - 1 } ),			
			
			TeamRoleList2 = proplists:delete(KickRoleid, TeamRoleList),
			PidListSendOther = proplists:delete(Roleid, PidList),
			PidListNew = proplists:delete(KickRoleid, PidList),
			
			NewRoleList = get_p_team_role_list( TeamRoleList2 ),
			P = #p_team_role_list{ role = NewRoleList },

			KickTeamRole = proplists:get_value(KickRoleid, TeamRoleList),
			KickRolename = KickTeamRole#p_team_role.rolename,
			
			SendSelf = #m_team_kick_toc{succ=true, list = P, roleid = KickRoleid, 
									rolename = KickRolename, teamid = TeamId},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"kick">>, SendSelf),
		
			SendMember = #m_team_kick_toc{return_self = false, list = P, roleid = KickRoleid, 
									rolename = KickRolename, teamid = TeamId},
			
			% ?TEST_MSG("Kick data: ~p", [{KickTeamRole, SendSelf, SendMember}]),
			lists:foreach( fun({_, MPid, _}) ->
				case mgee_misc:get_socket_by_rolepid(MPid) of
					{ok, Sock2} ->
						mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"kick">>, SendMember);
					_ ->
						ingore
				end
			end, PidListSendOther),
	
			State#mod_team_status{
						 team_role_list=TeamRoleList2,
						 pid_list = PidListNew
						} ;	
		_ ->
			?ERROR_MSG("unknown error",[]),
			State
	end,
	?TEST_MSG("team do_kick, new status: ~p",[StateNew]),
    {noreply, StateNew};

handle_cast( {do_change_leader, ClientSock, Roleid, _RoleName, TeamId, ToRoleid, ToRoleName}, State) ->
	#mod_team_status{pid_list = PidList, team_role_list = TeamRoleList} = State,
	?TEST_MSG("team do_change_leader, status: ~p",[State]),
	Result1 =
	case proplists:lookup(Roleid, TeamRoleList) of
		{Roleid, TeamRole} ->
			if TeamRole#p_team_role.is_leader ->
				if erlang:length(TeamRoleList) > 1 ->
					   
					% change leader to next role.
					case proplists:lookup(ToRoleid, TeamRoleList) of
						{ToRoleid, _} ->
							ok;
						_ ->
							{false, ?_LANG_TEAM_CHANGE_LEADER_FAIL_NOT_IN2}
					end;
				   
				true ->
					% the team only one, cann't change_leader
					{false, ?_LANG_TEAM_CHANGE_LEADER_FAIL_ONLY_ONE}
				end;   
			true ->
				% must leader authority 
				{false, ?_LANG_TEAM_LEADER_AUTHORITY}
			end;
		_ ->
			{false, ?_LANG_TEAM_NOT_IN}
	end,
	?TEST_MSG("team do_change_leader, result 1: ~p",[Result1]),
	StateNew = 
	case Result1 of 
		{false, Reason} ->
			SendSelf = #m_team_change_leader_toc{succ=false, reason = Reason},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"change_leader">>, SendSelf),
			State;
		ok ->
			
			TeamRoleList2 = change_leader_team_role_list(TeamRoleList, Roleid, ToRoleid),
			?TEST_MSG("team do_change_leader, change_leader_team_role_list: ~p",[TeamRoleList2]),
		
			PidList2 = proplists:delete(Roleid, PidList),
			
			NewRoleList = get_p_team_role_list( TeamRoleList2 ),
			P = #p_team_role_list{ role = NewRoleList },

			SendSelf = #m_team_change_leader_toc{succ=true, list = P, 
											roleid = ToRoleid, rolename = ToRoleName, teamid= TeamId},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"change_leader">>, SendSelf),

			SendMember = #m_team_change_leader_toc{return_self = false, list = P, 
											roleid = ToRoleid, rolename = ToRoleName, teamid= TeamId},
			lists:foreach( fun({_, MPid, _}) ->
					case mgee_misc:get_socket_by_rolepid(MPid) of
						{ok, Sock2} ->
							mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"change_leader">>, SendMember);
						_ ->
							ingore
					end
				end, PidList2),
			
			State#mod_team_status{
							 team_role_list=TeamRoleList2
							};
		_ ->
			?ERROR_MSG("unknown error",[]),
			State
	end,
	?TEST_MSG("team do_change_leader, new status: ~p",[StateNew]),
    {noreply, StateNew};

handle_cast(_Msg, State)-> 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% 
%% %% server is shutdown
%% handle_info({'EXIT', _Pid, shutdown}, State) ->
%% 	?DEBUG("do_client function has quited with reason shutdown", []),
%% 	{stop, "do_client function has quited with reason shutdown", State};
%% %% receive data timeout
%% handle_info({'EXIT', _Pid, normal}, State) ->
%% 	?DEBUG("the client auth process has quit with reason normal", []),
%% 	{noreply, State};
%% %% any other reason caused quit
%% handle_info({'EXIT', _Pid, Reason}, State) ->
%% 	?DEBUG("do_client function has quited with reason ~p", [Reason]),
%% 	{stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
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

get_next_team_order_num(TeamRoleList) ->
	CurMax = lists:max(
				lists:map(
					fun( {_Roleid,T} ) -> T#p_team_role.team_order
					end, TeamRoleList
					)
			),
	?TEST_MSG("get_next_team_order_num, cur_max: ~p", [CurMax]),
	(CurMax + 1).

% @doc trans [{roleid, p_team_role}, {roleid, p_team_role},...] to [p_team_role, p_team_role,...]
get_p_team_role_list( TeamRoleList ) ->
	lists:map( 
		fun({_Roleid, T}) ->
			T
		end, TeamRoleList).



change_leader_team_role_list(TeamRoleList, Roleid, ToRoleid) ->
	TeamRole1 = proplists:get_value(Roleid, TeamRoleList),
	LeaderOrder = TeamRole1#p_team_role.team_order,

	Role4 = proplists:get_value(ToRoleid, TeamRoleList),
	MemberFollow = Role4#p_team_role.is_follow,

	TeamRole2 = TeamRole1#p_team_role{is_leader = false, 
									team_order = get_next_team_order_num(TeamRoleList),
									is_follow = MemberFollow } ,
	% leader cann't follow others.
	Role5 = Role4#p_team_role{is_leader = true, team_order = LeaderOrder, is_follow = false } ,

	TeamRoleList2 = proplists:delete(Roleid, TeamRoleList),
	TeamRoleList3 = [ {Roleid, TeamRole2} | TeamRoleList2],
	TeamRoleList4 = proplists:delete(ToRoleid, TeamRoleList3),
	TeamRoleList5 = [ {ToRoleid, Role5} | TeamRoleList4],
	TeamRoleList5.


% @doc find next person who can become team leader,
%  sort the list, and get the second order's data.
find_next_team_leader(TeamRoleList) ->
	if (length(TeamRoleList) > 1) ->
		SortList = lists:sort(
				lists:map(
					fun( {_Roleid,T} ) -> {T#p_team_role.team_order, T}
					end, TeamRoleList
					)
			),
		{TeamOrder, TeamRole} = lists:nth( 2, SortList),
		{ok, TeamOrder, TeamRole};	
	true ->
		{error, role_too_less}
	end.

% @doc a role leave the team, maybe exit the game, or disconnect  
do_leave_member(ClientSock, Roleid, RoleName, State ) ->
	#mod_team_status{teamid=TeamId, pid_list = PidList, team_role_list = TeamRoleList} = State,

	% notice mod_team_server to change the ets data.
	% this role succ leave team, then must delete roleid at this teamid.
	gen_server:cast(mod_team_server, {delete_role_at_team, Roleid, TeamId, length(TeamRoleList) - 1 } ),			
			
	TeamRoleList2 = proplists:delete(Roleid, TeamRoleList),
	PidList2 = proplists:delete(Roleid, PidList),
			
	NewRoleList = get_p_team_role_list( TeamRoleList2 ),
	P = #p_team_role_list{ role = NewRoleList },

	case is_port(ClientSock) of
	   true ->
			SendSelf = #m_team_leave_toc{succ=true, teamid= TeamId},
			mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"leave">>, SendSelf);
		_ ->
			ingore
	end,
	
	SendMember = #m_team_leave_toc{return_self = false, list = P, 
									roleid = Roleid, rolename = RoleName, teamid = TeamId},
	lists:foreach( fun({_, MPid, _}) ->
			case mgee_misc:get_socket_by_rolepid(MPid) of
				{ok, Sock2} ->
					mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"leave">>, SendMember);
				_ ->
					ingore
			end
		end, PidList2),
	
	{ok, State#mod_team_status{
						 team_role_list=TeamRoleList2,
						 pid_list = PidList2
						} }
	.


% @doc when leader leave team, it must auto change leader to next role.
% team leader leave, must change leader position to next person.
do_leave_leader(ClientSock, Roleid, RoleName, State ) ->
	#mod_team_status{teamid=TeamId, pid_list = PidList, team_role_list = TeamRoleList} = State,

	case find_next_team_leader(TeamRoleList) of
		{ok, _ChooseTeamOrder, ChooseTeamRole} ->
			% notice mod_team_server to change the ets data.
			% this role succ leave team, then must delete roleid at this teamid.
			gen_server:cast(mod_team_server, {delete_role_at_team, Roleid, TeamId, length(TeamRoleList) - 1 } ),			
			
			TeamRoleList2 = change_leader_team_role_list(TeamRoleList, Roleid, 
														 ChooseTeamRole#p_team_role.roleid),
			?TEST_MSG("team do_leave, leave_leader, change_leader_team_role_list: ~p",[TeamRoleList2]),
			TeamRoleList3 = proplists:delete(Roleid, TeamRoleList2),			
			PidList2 = proplists:delete(Roleid, PidList),
			
			NewRoleList = get_p_team_role_list( TeamRoleList3 ),
			P = #p_team_role_list{ role = NewRoleList },

			case is_port(ClientSock) of
				true ->
					SendSelf = #m_team_leave_toc{succ=true, teamid= TeamId},
					mgee_packet:packet_encode_send2(ClientSock, <<"team">>, <<"leave">>, SendSelf);
				_ ->
					ingore
			end,

			SendMember = #m_team_leave_toc{return_self = false, list = P, 
											roleid = Roleid, rolename = RoleName, teamid = TeamId},
			lists:foreach( fun({_, MPid, _}) ->
					case mgee_misc:get_socket_by_rolepid(MPid) of
						{ok, Sock2} ->
							mgee_packet:packet_encode_send2(Sock2, <<"team">>, <<"leave">>, SendMember);
						_ ->
							ingore
					end
				end, PidList2),

			{ok, State#mod_team_status{
							 team_role_list=TeamRoleList3,
							 pid_list = PidList2
							} };
		
		{error, role_too_less} ->
			% disband the team
			{ok, stop}
	end
	.

