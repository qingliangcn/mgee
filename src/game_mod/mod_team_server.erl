%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-19
%%% @doc  team server.
%%% @end
%%%----------------------------------------------------------------------

-module(mod_team_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
-include("global_lang.hrl").

-export([start/0, start_link/0]).
-export([get_info/0,
		 get_info/1 
		]).
-export([
		 handle/1,
		 role_exit_game/3
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ETS_ALL_TEAM_LIST, mgee_ets_all_team_list).
-define(ETS_ROLEID_AT_TEAM, mgee_ets_roleid_at_team).

-record( mod_team_server_status, { last_team_id } ).

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

start() ->
	{ok, _} = supervisor:start_child(
	  mgee_sup, 
	  {mod_team_sup,
	   {mod_team_sup, start_link, []},
	   transient, infinity, supervisor, [mod_team_sup]}),
	{ok, _} = supervisor:start_child(
	  mgee_sup, 
	  {mod_team_server,
	   {mod_team_server, start_link, []},
	   transient, brutal_kill, worker, [mod_team_server]}),
	ok.

%% start this server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc get module info
get_info() ->
	gen_server:call(?MODULE, info).

get_info(TeamId) ->
	gen_server:call(?MODULE, {info, TeamId}).

handle({ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName}) ->
	?TEST_MSG("~p ~p ~p", [Module, Method, Data]),
	case Method of
				<<"follow">> ->
					TeamId = Data#m_team_follow_tos.teamid,
					SetFollow = Data#m_team_follow_tos.set_follow,
					gen_server:call(?MODULE, {follow, ClientSock, Roleid, RoleName, TeamId, SetFollow});
				<<"list">> ->
					gen_server:call(?MODULE, {list, ClientSock, Roleid, RoleName});
				<<"create">> ->
					gen_server:call(?MODULE, {create, ClientSock, Roleid, RoleName});
				<<"invite">> ->
					InviteRoleid = Data#m_team_invite_tos.roleid,
					gen_server:call(?MODULE, {invite, ClientSock, Roleid, RoleName, InviteRoleid});
				<<"accept">> ->
					TeamId = Data#m_team_accept_tos.teamid,
					gen_server:call(?MODULE, {accept, ClientSock, Roleid, RoleName, TeamId});
				<<"refuse">> ->
					TeamId = Data#m_team_refuse_tos.teamid,
					gen_server:call(?MODULE, {refuse, ClientSock, Roleid, RoleName, TeamId});
				<<"leave">> -> 
					TeamId = Data#m_team_leave_tos.teamid,
					gen_server:call(?MODULE, {leave, ClientSock, Roleid, RoleName, TeamId});
				<<"kick">> -> 
					KickRoleid = Data#m_team_kick_tos.roleid,
					gen_server:call(?MODULE, {kick, ClientSock, Roleid, RoleName, KickRoleid});
				<<"offline">> ->
					gen_server:call(?MODULE, {offline, ClientSock, Roleid, RoleName});
				<<"change_leader">> ->
					TeamId = Data#m_team_change_leader_tos.teamid,
					ToRoleid = Data#m_team_change_leader_tos.roleid,
					ToRoleName = Data#m_team_change_leader_tos.rolename,
					gen_server:call(?MODULE, {change_leader, ClientSock, Roleid, RoleName, TeamId, ToRoleid, ToRoleName});
				Other -> 
					?ERROR_MSG("not implemented method of ~p :~p", [?MODULE, Other]),
					not_implemented
	end.

% @doc when role exit game, call this func to do something.
role_exit_game(Roleid, RoleName, RolePid) ->
	?TEST_MSG("role_exit_game: ~p ~p ~p", [Roleid, RoleName, RolePid]),
	gen_server:cast(?MODULE, {role_exit_game, Roleid, RoleName, RolePid} ),
	ok.

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	?INFO_MSG("~p init: ~p",[?MODULE, [] ]),
	ets:new(?ETS_ALL_TEAM_LIST, [set, protected, named_table]),
	ets:new(?ETS_ROLEID_AT_TEAM, [set, protected, named_table]),
	
	% ETS_ROLEID_AT_TEAM struc:  [ {roleid, teamid},  {roleid, teamid}, {roleid, teamid}, ....]
	% ETS_ROLEID_AT_TEAM use for found roleid at which team.
	%
	% ETS_ALL_TEAM_LIST struc:   [ {teamid, teamdesc, pid, member_count}, {teamid, teamdesc, pid, member_count}, {teamid, teamdesc, pid, member_count}, ....]
	%	
	% add one record, use for  ets:update_counter()
	% and auto incr teamid (like mysql primary key). the new teamid use for create team event.
	ets:insert(?ETS_ALL_TEAM_LIST, {ets_update_counter_team_id_incr, 0}),
    {ok, #mod_team_server_status{last_team_id = 0}}.

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


% @doc follow the team leader's walk... 
handle_call( {follow, ClientSock, Roleid, RoleName, TeamId, SetFollow}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid, TeamId) of
		{ok, Roleid, TeamId , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_follow, ClientSock, Roleid, RoleName, TeamId, SetFollow} ),
			ok;
		_ -> 
			#m_team_follow_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("follow team leader, ~p", [Reply]),
    {reply, Reply, State};

% @doc handle list event call
handle_call( {list, ClientSock, Roleid, RoleName}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, _ , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_list, ClientSock, Roleid, RoleName}),
			ok;
		_ -> 
			#m_team_list_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("list team roles, ~p", [Reply]),
    {reply, Reply, State};

% @doc create a new team, only 1 person at team.
handle_call( {create, _ClientSock, Roleid, RoleName}, _From, State) ->
	Result = 
	case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, TeamId , Pid} when is_pid(Pid) ->
			?TEST_MSG("get_team_pid_by_roleid: ~p result:~p",[Roleid, Pid]),
			{ok, TeamId, gen_server:call(Pid, list) };
		_ -> 
			?TEST_MSG("get_team_pid_by_roleid: ~p not found",[Roleid]),
			case create_team(Roleid, RoleName) of
				{ok, TeamID, Pid2} when is_pid(Pid2) ->
					{ok, TeamID, gen_server:call(Pid2, list) };
				_ ->
					{false, ?_LANG_TEAM_CREATE_FAIL }
			end
	end,
	?TEST_MSG("create team result: ~p", [Result]),
	Reply = 
	case Result of
		{ok, TeamID2, PTRL} when is_record(PTRL, p_team_role_list) ->
			#m_team_create_toc{succ=true, list=PTRL, teamid=TeamID2 };
		{false, Reason} ->
			#m_team_create_toc{succ=false, reason=Reason };
		_ ->
			#m_team_create_toc{succ=false, reason= mgee_tool:to_list(Result) }
	end,
	?TEST_MSG("create team, ~p", [Reply]),
    {reply, Reply, State};

% @doc invite 
handle_call( {invite, ClientSock, Roleid, RoleName, InviteRoleid}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, _ , TeamPid} when is_pid(TeamPid) ->
			gen_server:cast(TeamPid, {do_invite, ClientSock, Roleid, RoleName,
								  			InviteRoleid, get_team_pid_by_roleid(InviteRoleid) } ),
			ok;
		_ -> 
			#m_team_invite_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("invite team, ~p", [Reply]),
    {reply, Reply, State};

% @doc accept and join in the team
handle_call( {accept, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	Reply = case get_team_pid_by_teamid(TeamId) of
		{ok, TeamId , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_accept, ClientSock, Roleid, RoleName, TeamId,
								  			get_team_pid_by_roleid(Roleid) } ),
			ok;
		_ -> 
			#m_team_accept_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("accept team, ~p", [Reply]),
    {reply, Reply, State};

% @doc refuse the invite 
handle_call( {refuse, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	Reply = case get_team_pid_by_teamid(TeamId) of
		{ok, TeamId , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_refuse, ClientSock, Roleid, RoleName, TeamId } ),
			ok;
		_ -> 
			ignore
%% 			#m_team_refuse_toc{succ=false,
%% 							   reason=?_LANG_TEAM_NOT_EXISTS  
%% 								}
	end,
	?TEST_MSG("refuse team, ~p", [Reply]),
    {reply, Reply, State};

% @doc leave/exit the team 
handle_call( {leave, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid, TeamId) of
		{ok, Roleid, TeamId , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_leave, ClientSock, Roleid, RoleName, TeamId } ),
			ok;
		_ -> 
			#m_team_leave_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("leave team, ~p", [Reply]),
    {reply, Reply, State};

% @doc the leader kick sameone out
handle_call( {kick, ClientSock, Roleid, RoleName, KickRoleid}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, _TeamId , Pid} when is_pid(Pid) ->
			gen_server:cast(Pid, {do_kick, ClientSock, Roleid, RoleName, KickRoleid } ),
			ok;
		_ -> 
			#m_team_leave_toc{succ=false,
							   reason=?_LANG_TEAM_NOT_EXISTS  
								}
	end,
	?TEST_MSG("kick team, ~p", [Reply]),
    {reply, Reply, State};


% @doc team member offline...
handle_call( {offline, ClientSock, Roleid, RoleName}, _From, State) ->
	Reply = case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, _TeamId , Pid} when is_pid(Pid) ->
			gen_server:call(Pid, {offline, ClientSock, Roleid, RoleName} );
		_ -> 
			ignore
	end,
	?TEST_MSG("offline team, ~p", [Reply]),
    {reply, Reply, State};


% @doc the team leader change 
handle_call( {change_leader, ClientSock, Roleid, RoleName, TeamId, ToRoleid, ToRoleName}, _From, State) ->
	Reply =
	case Roleid of
		ToRoleid ->
			#m_team_change_leader_toc{	succ=false,
							   			reason=?_LANG_TEAM_CHANGE_LEADER_FAIL_TO_SELF  
									 };
		_ ->
			case get_team_pid_by_roleid(Roleid, TeamId) of
				{ok, Roleid, TeamId , Pid} when is_pid(Pid) ->
					gen_server:cast(Pid, {do_change_leader, ClientSock, Roleid, RoleName, TeamId, ToRoleid, ToRoleName} ),
					ok;
				_ -> 
					#m_team_change_leader_toc{	succ=false,
							   			reason=?_LANG_TEAM_NOT_EXISTS  
									 }
			end
	end,
	?TEST_MSG("change_leader team, ~p", [Reply]),
    {reply, Reply, State};


% @doc handle get info
handle_call( info, _From, State) ->
	#mod_team_server_status{last_team_id = LastId} = State,
	Reply = { LastId, 
			  ?ETS_ROLEID_AT_TEAM, ets:info(?ETS_ROLEID_AT_TEAM), ets:tab2list(?ETS_ROLEID_AT_TEAM),
			  ?ETS_ALL_TEAM_LIST, ets:info(?ETS_ALL_TEAM_LIST), ets:tab2list(?ETS_ALL_TEAM_LIST)
			  },
	{reply, Reply, State};

handle_call( {info, TeamId}, _From, State) ->
	Reply = case get_team_pid_by_teamid(TeamId) of
		{ok, TeamId , Pid} when is_pid(Pid) ->
			gen_server:call(Pid, {info, TeamId } );
		_ -> 
			%{error, not_found}
			List = lists:foldl(
			  	fun(H, T) ->
					case H of
						{Tid, _, _, Count} -> [{Tid,Count} | T];
						_ 			-> T
					end			  
				end, [], ets:tab2list( ?ETS_ALL_TEAM_LIST )),
			lists:sort(List)
	end,
    {reply, Reply, State};

handle_call(Request, From, State) ->
	?DEBUG("~p handle_call from ~p : ~p", [?MODULE, From, Request]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% @doc notice mod_team_server to change the ets data.
handle_cast( {update_role_team, Roleid, _RoleName, TeamId, NewMemberCount } , State ) ->
	ets:insert(?ETS_ROLEID_AT_TEAM, {Roleid, TeamId}),	
	case ets:lookup(?ETS_ALL_TEAM_LIST, TeamId) of
		[{TeamId, TeamDesc, Pid, _ }] ->
			ets:insert(?ETS_ALL_TEAM_LIST, {TeamId, TeamDesc, Pid, NewMemberCount}),
			?TEST_MSG("teamid: ~p now have ~p members.",[TeamId, NewMemberCount]);
		_ ->
			not_found
	end,
	{noreply, State};

handle_cast( {delete_role_at_team, Roleid, TeamId, NewMemberCount } , State ) ->
	ets:delete(?ETS_ROLEID_AT_TEAM, Roleid),
	case ets:lookup(?ETS_ALL_TEAM_LIST, TeamId) of
		[{TeamId, TeamDesc, Pid, _ }] ->
			ets:insert(?ETS_ALL_TEAM_LIST, {TeamId, TeamDesc, Pid, NewMemberCount}),
			?TEST_MSG("teamid: ~p now have ~p members.",[TeamId, NewMemberCount]);
		_ ->
			not_found
	end,
	{noreply, State};

handle_cast( {delete_team, Teamid}, State) ->
	ets:delete(?ETS_ALL_TEAM_LIST, Teamid),
  	{noreply, State};


handle_cast( {role_exit_game, Roleid, RoleName, RolePid}, State) ->
	case get_team_pid_by_roleid(Roleid) of
		{ok, Roleid, TeamId , TeamPid} when is_pid(TeamPid) ->
			ClientSock = case mgee_misc:get_socket_by_rolepid(RolePid) of
							 {ok, Sock} -> Sock;
							 _ -> none
						 end,
			gen_server:cast(TeamPid, {do_leave, ClientSock, Roleid, RoleName, TeamId } ),
			ok;
		_ -> 
			ingore
	end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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

% @doc roleid auto incr one every call this func.
get_new_team_id() ->
	ets:update_counter(?ETS_ALL_TEAM_LIST, ets_update_counter_team_id_incr, 1).

get_team_pid_by_teamid(TeamId) ->
	?TEST_MSG("get_team_pid_by_teamid: ~p",[TeamId]),
	case ets:lookup(?ETS_ALL_TEAM_LIST, TeamId) of
		[{TeamId, _TeamDesc, Pid, _}] ->
			?TEST_MSG("get_team_pid_by_roleid found team pid: ~p",[Pid]),
			{ok, TeamId , Pid};
		_ ->
			not_found
	end.
	

% @doc found roleid at which team.
get_team_pid_by_roleid(Roleid) ->
	?TEST_MSG("get_team_pid_by_roleid: ~p",[Roleid]),
	case ets:lookup(?ETS_ROLEID_AT_TEAM, Roleid) of
		[{Roleid,  TeamId }] ->
			?TEST_MSG("get_team_pid_by_roleid found teamid: ~p",[TeamId]),
			case ets:lookup(?ETS_ALL_TEAM_LIST, TeamId) of
				[{TeamId, _TeamDesc, Pid, _}] ->
					?TEST_MSG("get_team_pid_by_roleid found team pid: ~p",[Pid]),
					{ok, Roleid,  TeamId , Pid};
				_ ->
					not_found
			end;
		_ ->
			not_found
	end.
get_team_pid_by_roleid(Roleid, TeamId) ->
	?TEST_MSG("get_team_pid_by_roleid: ~p ~p",[Roleid, TeamId]),
	case ets:lookup(?ETS_ROLEID_AT_TEAM, Roleid) of
		[{Roleid,  TeamId }] ->
			?TEST_MSG("get_team_pid_by_roleid found teamid: ~p",[TeamId]),
			case ets:lookup(?ETS_ALL_TEAM_LIST, TeamId) of
				[{TeamId, _TeamDesc, Pid, _}] ->
					?TEST_MSG("get_team_pid_by_roleid found team pid: ~p",[Pid]),
					{ok, Roleid,  TeamId , Pid};
				_ ->
					not_found
			end;
		_ ->
			not_found
	end.
	

% @doc create a new team
create_team(Roleid, RoleName) ->
	NewTeamID = get_new_team_id(),
	TeamDesc = "team_" ++ integer_to_list(NewTeamID) ++ ", create by " ++ mgee_tool:to_list(RoleName),
	case supervisor:start_child(mod_team_sup, [{NewTeamID, TeamDesc, Roleid, RoleName}]) of
		{ok, Pid} ->
			ets:insert(?ETS_ALL_TEAM_LIST, {NewTeamID, TeamDesc, Pid, 1}),
			ets:insert(?ETS_ROLEID_AT_TEAM, {Roleid, NewTeamID}),
			?DEBUG("create new team succ, ~p", [{NewTeamID, TeamDesc, Pid}]),
			{ok, NewTeamID, Pid};
%% 		{error, {alreay_started, Pid}} -> 
%% 			Pid;
		R ->
			?DEBUG("create new team fail, ~p", [{NewTeamID, TeamDesc, R}]),
			{false, fail_satrt_child}
	end.
	

