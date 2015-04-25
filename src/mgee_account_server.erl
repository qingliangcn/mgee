%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-13
%%% @doc TODO: Add description to mod_account_manager
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_account_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("global_lang.hrl").
-include("game_pb.hrl").

-export([start/0, start_link/0]).
-export([get_info/0]).
-export([
		 handle/1,
		 create_account/1,
		 list_role/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record( mod_account_manager_status, {ets_acc, ets_role, last_role_id } ).

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

start() ->
	{ok, _} = supervisor:start_child(
	  mgee_sup, 
	  {mgee_account_sup,
	   {mgee_account_sup, start_link, []},
	   permanent, infinity, supervisor, [mgee_account_sup]}),
	{ok, _} = supervisor:start_child(
	  mgee_sup, 
	  {mgee_account_server,
	   {mgee_account_server, start_link, []},
	   transient, brutal_kill, worker, [mgee_account_server]}),
	ok.

%% start this server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], ?GEN_SERVER_OPTIONS).

% @doc get module info
get_info() ->
	gen_server:call(?MODULE, info).

handle({_ClientSock, Module, Method, Data, AccountName, _Roleid, _RoleName}) ->
	?TEST_MSG("~p ~p ~p", [Module, Method, Data]),
	case Method of
		<<"flash_login">> ->
			List = gen_server:call(?MODULE, {list_no_binary, AccountName}),
			?TEST_MSG("~p", [List]),
			#m_login_flash_login_toc{result=#p_role_list{role=List}};
		<<"list">> ->
			mgee_account_server:list_role(AccountName);
		<<"add">> -> 
			RoleName = Data#m_role_add_tos.rolename,
			Sex = Data#m_role_add_tos.sex,
			SkinId = Data#m_role_add_tos.skinid,
			gen_server:call(?MODULE, {add, AccountName, RoleName, Sex, SkinId});
		<<"del">> ->
			gen_server:call(?MODULE, {del, AccountName, Data#m_role_del_tos.roleid});
		<<"create_acount">> ->
			mgee_account_server:create_account(AccountName);
		<<"enter">> ->
			AccountProcessName = mgee_misc:account_process_name(AccountName),
			gen_server:call(AccountProcessName, 
							{enter, Module, Method, Data#m_role_enter_tos.roleid});
		Other -> 
			?ERROR_MSG("not implemented method of account_server :~p", [Other]),
			not_implemented
	end.

create_account(AccountName) ->
	gen_server:call(?MODULE, {create_account, AccountName}).

list_role(AccountName) ->
	gen_server:call(?MODULE, {list, AccountName}).

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
	%% start a ets to save account data
	%% @todo: read account data and role base data from mgee_persistent module( maybe it is mysql ).
	%% 
	EtsAccList = ets:new(ets_account, [set, protected, named_table]),
	EtsRoleList = ets:new(ets_account_rolename, [set, private]),
	
	% EtsRoleList struc:  [ {rolename, roleid},  {rolename, roleid},  {rolename, roleid}, ....]
	%		and auto incr roleid (like mysql primary key). the new roleid use for add role event.
	% EtsRoleList use for check rolename exist.
	%
	% EtsAccList struc:   [ {account_name, rolelist}, {account_name, rolelist}, {account_name, rolelist}, ....]
	% rolelist struc: [ p_game_role, p_game_role, p_game_role, ....]
	% p_game_role is a record define at file game_pb.hrl
	% 		it contains  roleid, rolename, sex, etc...
	%
	
	% add one record, use for  ets:update_counter()
	ets:insert(EtsRoleList, {ets_update_counter_role_id_incr, 0}),
    {ok, #mod_account_manager_status{ets_acc=EtsAccList, ets_role=EtsRoleList, last_role_id = 0}}.

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


handle_call({create_account, AccountName}, _From, State) ->
	AccountEts = State#mod_account_manager_status.ets_acc,
	Reply = create_account(AccountEts, AccountName),
	{reply, Reply, State};


% @doc handle list role event call
handle_call( {list, AccountName }, _From, State) ->
	#mod_account_manager_status{ets_acc=EtsAccList} = State,	
	DataBin = case ets:lookup(EtsAccList, AccountName) of
					[{AccountName,  RoleList }] ->
						  #m_role_list_toc{result=RoleList};
					_ ->
						  #m_role_list_toc{reason=?_LANG_ACCOUNT_NOT_EXISTS}
			   end,
    {reply, DataBin, State};

handle_call({list_no_binary, AccountName}, _From, State) ->
	#mod_account_manager_status{ets_acc=EtsAccList} = State,
	Reply = case ets:lookup(EtsAccList, AccountName) of
				[{AccountName,  RoleList }] -> 
					RoleList;
				[] -> []
			end,
	{reply, Reply, State};

% @doc handle add role event call
handle_call( {add, AccountName, RoleName, Sex, SkinId}, 
			 _From, State) ->
	#mod_account_manager_status{ets_acc=EtsAccList, ets_role=EtsRoleList} = State,	
	% check if this RoleName had used.
	Result = case ets:lookup(EtsRoleList, RoleName) of
		[{ RoleName, _ }] -> 
			?TEST_MSG("role ~p already exist.", [RoleName]),
			{error, role_name_exist};
		_ ->
		case ets:lookup(EtsAccList, AccountName) of
			[{AccountName,  RoleList }] ->
				?TEST_MSG("ets lookup [~p, ~p] result: ~p", [EtsAccList, AccountName, RoleList]),
				%RoleList = #game_account{account_name = AccountName, rolelist = RoleList}
				if erlang:length(RoleList) >= ?ACCOUNT_ROLE_COUNT_MAX ->
					{error, role_max_limit};
				true ->
					NewRoleID = get_new_role_id( EtsRoleList ),
					ets:insert(EtsRoleList, { RoleName, NewRoleID } ),
					% add role
					RoleListNew = [ #p_game_role{
										roleid=NewRoleID, 
										rolename=RoleName, 
										sex=Sex, 
										skinid=SkinId,
										vwid=100001,
										x=12,
										y=34} 
									| RoleList ],
					?TEST_MSG("add more role [~p] for account [~p]", [RoleName, AccountName]),
					ets:insert(EtsAccList, { AccountName, RoleListNew} ),
    				RoleListNew				
				end;
			R -> 
				?TEST_MSG("ets lookup [~p, ~p] result: ~p", [EtsAccList, AccountName, R]),
				NewRoleID = get_new_role_id( EtsRoleList ),
				ets:insert(EtsRoleList, {RoleName, NewRoleID} ),
				% add role
				RoleListNew = [#p_game_role{roleid=NewRoleID, rolename=RoleName, sex=Sex, skinid=SkinId}],
				?TEST_MSG("add first role [~p] for account [~p]", [RoleName, AccountName]),
				ets:insert(EtsAccList, {AccountName, RoleListNew} ),				
				RoleListNew			
		end
	end,
	DataBin = case Result of
				{error, role_max_limit} -> 
					#m_role_add_toc{succ=false, reason=?_LANG_ROLE_MAX_COUNT_LIMIT};
				{error, role_name_exist} ->
					#m_role_add_toc{succ=false, reason=?_LANG_ROLE_NAME_EXIST};
				Lists when is_list(Lists) -> 
					#m_role_add_toc{result=#p_role_list{role=Lists}};
			  	_ ->
					?TEST_MSG("unexpected result ~p", [Result])
			end,
    {reply, DataBin, State};


% @doc handle delete role event call
handle_call( {del, AccountName, RoleId}, _From, State) ->
	#mod_account_manager_status{ets_acc=EtsAccList, ets_role=EtsRoleList} = State,	
	Result = case ets:lookup(EtsAccList, AccountName) of
		[{AccountName,  RoleList }] ->
			?TEST_MSG("ets lookup [~p, ~p] result: ~p", [EtsAccList, AccountName, RoleList]),
			{ DelRoleList, RoleListNew} = lists:partition( fun(T) -> T#p_game_role.roleid =:= RoleId end, RoleList),
			% check if this RoleId exist and owner is AccountName
			if erlang:length(DelRoleList) =:= 0 ->
				?TEST_MSG("del role error, not found this roleid [~p] at account [~p]", [RoleId, AccountName]),
				{error, role_not_exist};
			true ->
				[DelRole] = DelRoleList,
				DelRoleName = DelRole#p_game_role.rolename,
				case ets:lookup(EtsRoleList, DelRoleName) of
					[{ DelRoleName, _ }] -> 
						?TEST_MSG("delete exist role name ~p", [DelRoleName]),
						ets:delete(EtsRoleList, DelRoleName ),
						ets:insert(EtsAccList, { AccountName, RoleListNew} ),
    					RoleListNew;
					_ ->
						?TEST_MSG("delete exist role name error, can't found role name.", []),
						{error, role_not_exist}
				end
			end;
		_ ->
			?TEST_MSG("del role error, not found this account [~p]", [AccountName]),
			{error, account_not_exist}
	end,
	DataBin = case Result of
					{error, role_not_exist} ->
						  #m_role_del_toc{succ=false, reason=?_LANG_ROLE_NOT_EXISTS};
					{error, account_not_exist} ->
						  #m_role_del_toc{succ=false,reason=?_LANG_ACCOUNT_NOT_EXISTS};
			  		Lists when is_list(Lists) ->
							#m_role_del_toc{result=#p_role_list{role=Lists}};
			  		_ ->
						?TEST_MSG("unexpected result ~p", [Result])
				 end,
    {reply, DataBin, State};


% @doc handle get info
handle_call( info, _From, State) ->
	#mod_account_manager_status{ets_acc=EtsAccList, ets_role=EtsRoleList, last_role_id = LastId} = State,
	Reply = { LastId, ets_role, ets:info(EtsRoleList), ets:tab2list(EtsRoleList),
			  ets_acc, ets:info(EtsAccList), ets:tab2list(EtsAccList)
			  },
	{reply, Reply, State};

handle_call( {update_display, AccountName, Roleid, RecordDisplay}, _From, State) 
  when is_record(RecordDisplay, p_game_role_disply) ->
	EtsAccList = State#mod_account_manager_status.ets_acc,
	Type = RecordDisplay#p_game_role_disply.type,
	case ets:lookup(EtsAccList, AccountName) of
		[{AccountName, RoleList}] -> 
			[Role] =
			lists:filter(
				fun(RoleInfo) ->
					RoleInfo#p_game_role.roleid =:= Roleid
				end, RoleList),
			TmpRoleList = lists:delete(Role, RoleList),
			OldDisplay = Role#p_game_role.display,
			if 
				OldDisplay =:= undefined ->
					NewDisplay = [RecordDisplay];
				true ->
					TmpDis = lists:filter(
					 	fun(T) ->
							T#p_game_role_disply.type =/= Type
						end, OldDisplay),
					NewDisplay = [RecordDisplay|TmpDis]
			end,
			NewRole = Role#p_game_role{display=NewDisplay},
			NewRoleList = [NewRole|TmpRoleList],
			ets:insert(EtsAccList, {AccountName, NewRoleList}),
			gen_server:cast(mgee_virtual_world_router, {update_display, Roleid, NewDisplay}),
			{reply, ok, State};
		[] ->
			{reply, error, State}
	end;
	

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
get_new_role_id( EtsRoleList ) ->
	ets:update_counter(EtsRoleList, ets_update_counter_role_id_incr, 1).

create_account(EtsTable, AccountName) ->
	case ets:lookup(EtsTable, AccountName) of
		[{AccountName, _}] -> 
			{error, ?_LANG_ACCOUNT_NAME_EXISTS};
		[] -> 
			ets:insert(EtsTable, {AccountName, []}),
			ok
	end.