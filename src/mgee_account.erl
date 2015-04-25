%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-13
%%% @doc every person use one mod_account gen_server to provide role data and money data,
%%%		when person login succ, create mod_account gen_server.
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_account).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("mgee.hrl").
-include("game_pb.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_info/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(ClientSock) ->
	gen_server:start_link(?MODULE, [ClientSock], ?GEN_SERVER_OPTIONS).

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
init([ClientSock]) ->
	?INFO_MSG("~p init: ~p",[?MODULE, [ClientSock] ]),
    process_flag(trap_exit, true),
    %todo, read account data and role list from mnesia.
    case inet:peername(ClientSock) of
	%% get client ip, may be we want to ban this ip or something else
	{ok, {Ip, _}} -> {ok, #account_state{client_sock = ClientSock, ip = Ip}};
	{error, ErrNo} -> {stop, inet:format_error(ErrNo)}
    end.


get_info(AccountName) ->
    gen_server:call(mgee_misc:account_process_name(AccountName), {info}).

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

%% @doc use a exist role to enter game
handle_call( {enter, Module, Method, Roleid }, _From, State ) ->
    %% send the detail information of the role to the client
    if 
	is_pid(State#account_state.role_pid) ->
	    clear_before_exit(State);
	true ->
	    none
    end,
    ClientSock = State#account_state.client_sock,
    RoleList = gen_server:call(mgee_account_server, 
			       {list_no_binary, State#account_state.account_name}),
    ?TEST_MSG("account rolelist ~p", [RoleList]),
    DataRtn = case get_role(Roleid, RoleList) of
		  [RoleBaseInfo] when is_record(RoleBaseInfo, p_game_role) -> 
		      ?TEST_MSG("find role ~p", [RoleBaseInfo]),
		      HpMax = mgee_tool:random(200, 300),
		      MpMax = mgee_tool:random(100, 200),
		      AttackMax = mgee_tool:random(10, 15),
		      DefenceMax = mgee_tool:random(5, 10),
		      AglieMax = mgee_tool:random(10, 15),
		      Level = mgee_tool:random(1, 100),
		      RoleAttr = #p_role_attr{
							roleid = Roleid,
							rolename = RoleBaseInfo#p_game_role.rolename,
							account_name = State#account_state.account_name,
							level = Level,
							hp = HpMax,
							mp = MpMax,
							attack = AttackMax,
							defence = DefenceMax,
							aglie = AglieMax,
							max_hp = HpMax,
							max_mp = MpMax,
							max_attack = AttackMax,
							max_defence = DefenceMax,
							max_aglie = AglieMax
		        },
				case mgee_role:start(Roleid, RoleBaseInfo, RoleAttr) of
					{ok, RolePid} ->
						pg2:join(pg2_all_role, RolePid),
						link(RolePid),
						?TEST_MSG("new role process ~p", [RolePid]),
						%% let the previous process exit
						exit(State#account_state.do_client_pid, after_enter),
						PidSelf = self(),
						PidSelf ! {event, check_heartbeat},
						DoClientPid = spawn_link(
							fun() -> 
								do_client_after_enter(State#account_state.account_name, 
								    Roleid, 
								    RoleBaseInfo#p_game_role.rolename, ClientSock, PidSelf) 
							end),
						?TEST_MSG("new do client process ~p", [DoClientPid]),
						NewState = State#account_state{
							cur_roleid = Roleid, 
							cur_rolename = RoleBaseInfo#p_game_role.rolename,
							role_pid = RolePid,
							last_heartbeat_time = mgee_timer:now(),
							do_client_pid = DoClientPid},
						#m_role_enter_toc{result=RoleAttr};
				  _ ->
						NewState = State,
						#m_role_enter_toc{succ=false, reason = <<"">>}					  
				end;
		  [] -> 
		      NewState = State,
		      #m_role_enter_toc{succ=false, reason = <<"">>}
	      end,
    mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRtn),
    {reply, ok, NewState};


handle_call( account_login_again, _From, State) ->
    ?DEBUG("account_login_again: ~p , so quit old process.", [State#account_state.account_name]),
    clear_before_exit(State),
    {stop, normal, ok, State};

handle_call({account_init, AccountName}, _From, State) ->
    mgee_account_server:create_account(AccountName),
    ?TEST_MSG("start account init :~p", [AccountName]),
    ClientName = mgee_misc:account_process_name(AccountName),
    %% if is global, we must be sure there is only one online at the same time
	
	NewState = 
    case mgee_misc:whereis_name({local, ClientName}) of
		undefined -> 
			mgee_misc:register(local, ClientName, self()),
			State#account_state{account_name=AccountName};
		Pid -> 
			 %% TODO: we need wait the sure msg here
			Re = gen_server:call(Pid, account_login_again),
			mgee_tool:sleep(1000),
			case Re of
				ok ->
					?DEBUG("call account_login_again ok", []);
				R ->
					?DEBUG("call account_login_again failed: ~p", [R])
			end,
			mgee_misc:register(local, ClientName, self()),
			State#account_state{account_name=AccountName}
    end,
    {reply, ok, NewState};


handle_call({info}, _From, State) ->
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({event, heartbeat}, State) ->
	?TEST_MSG("heartbeat ~p", [State#account_state.account_name]),
	Now = mgee_timer:now(),
	{noreply, State#account_state{last_heartbeat_time=Now}};

	
handle_info({event, check_heartbeat}, State) ->
	?TEST_MSG("check heartbeat ~p", [State#account_state.account_name]),
	Now = mgee_timer:now(),
	LastHeartBeat = State#account_state.last_heartbeat_time,
	Diff = timer:now_diff(Now, LastHeartBeat),
	case Diff > ?HEARTBEAT_TICKET_TIME of
		true ->
			case State#account_state.heartbeat_fail_num >= ?HEARTBEAT_MAX_FAIL_TIME of
				true ->
					%%{stop, normal, State};
					{noreply, State};
				false ->
					timer:send_after(?HEARTBEAT_TICKET_TIME, {event, check_heartbeat}),
					{noreply, State#account_state{heartbeat_fail_num=(State#account_state.heartbeat_fail_num)+1}}
			end;
		false ->
			timer:send_after(?HEARTBEAT_TICKET_TIME, {event, check_heartbeat}),
			{noreply, State}
	end;


%% socket closed
handle_info({'EXIT', _Pid, closed}, State) ->
    ?DEBUG("the client socket connnection has closed, we need to save some data!!!", []),
    clear_before_exit(State),
    {stop, normal, State};


%% server is shutdown
handle_info({'EXIT', Pid, shutdown}, State) ->
    ?DEBUG("~p shutdown", [Pid]),
    clear_before_exit(State),
    {stop, normal, State};

%% auth_failed
handle_info({'EXIT', Pid, auth_failed}, State) ->
    ?DEBUG("~p shutdown", [Pid]),
    clear_before_exit(State),
    {stop, normal, State};


%% receive data timeout
handle_info({'EXIT', _Pid, timeout}, State) ->
    ?DEBUG("do_client function has quited with reason : timeout", []),
    clear_before_exit(State),
    {stop, normal, State};


handle_info({'EXIT', _Pid, other_login}, State) ->
    ?DEBUG("do_client function has quited with reason : other_login", []),
    clear_before_exit(State),
    {stop, normal, State};

handle_info({'EXIT', _Pid, hack_attemp}, State) ->
    ?INFO_MSG("mgee_account ~p has quited with reason : hack_attemp", [State#account_state.account_name]),
    clear_before_exit(State),
    {stop, normal, State};


handle_info({'EXIT', Pid, after_enter}, State) ->
    ?DEBUG("do_client function ~p has quited with reason : after_enter", [Pid]),
    {noreply, State};


handle_info({'EXIT', Pid, normal}, State) ->
    ?DEBUG("the client auth process ~p has quit with reason : normal", [Pid]),
    {noreply, State};


handle_info({'EXIT', Pid, terminate}, State) ->
    ?DEBUG("the client auth process ~p has quit with reason : terminate", [Pid]),
    {noreply, State};


%% any other reason caused quit
handle_info({'EXIT', Pid, Reason}, State) ->
    ?DEBUG("~p has quited with reason ~p", [Pid, Reason]),
    clear_before_exit(State),
    {stop, normal, State};


handle_info({event, start_client}, State) ->
    ClientSock = State#account_state.client_sock,
    Pid = self(),
    DoClientPid = spawn_link(fun() -> do_client_spawn(Pid, ClientSock) end),
    NewState = State#account_state{do_client_pid = DoClientPid},
    {noreply, NewState};


handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	?INFO_MSG("mgee_account ~p terminated, socket ~p close now.", 
			  [State#account_state.account_name, State#account_state.client_sock]),
	case State#account_state.client_sock of
		undefined -> ok;
		Socket -> gen_tcp:close(Socket)
	end,
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
do_client_spawn(Parent, ClientSock) ->
    case gen_tcp:recv(ClientSock, 23, ?RECV_TIMEOUT) of
	{ok, ?CROSS_DOMAIN_FLAG} -> mgee_cross_domain:send_file(ClientSock);
	{ok, Handshaking} -> ?TEST_MSG("receive handshaking data ~p ", [Handshaking]),
			     do_client_auth(Parent, ClientSock);
	{error, Reason} -> exit(Reason)
    end.

do_client_auth(Parent, ClientSock) ->
    case mgee_auth:auth_user(ClientSock) of
	{passed, LoginMethod, AccountName} -> 
	    start_account_process(Parent, AccountName, ClientSock, LoginMethod);
	{failed, LoginMethod, Reason} -> 
	    DataRecord = #m_login_flash_login_toc{succ=false, reason=Reason},
	    mgee_packet:packet_encode_send(ClientSock, ?LOGIN_MODULE, LoginMethod, DataRecord),
	    exit(auth_failed);
	{error, Reason} -> 
		exit(Reason)
    end.

start_account_process(Parent, AccountName, ClientSock, LoginMethod) -> 
    case gen_server:call(Parent, {account_init, AccountName}) of
	ok -> 
	    mgee_router:router({ClientSock, ?LOGIN_MODULE, LoginMethod, <<>>, AccountName, none, none}),
	    do_client(AccountName, ClientSock);
	error -> 
	    error
    end.

%% after the client authed , we start this fun as a process to receive data from it
-spec do_client(AccountName :: atom(), ClientSock :: port()) -> no_return().
%% TODO need timeout
do_client(AccountName, ClientSock) ->
    case mgee_packet:recv(ClientSock) of
		{ok, heartbeat} ->
			ignore,
			do_client(AccountName, ClientSock);
		{ok, {Module, Method, Data}} -> 
	    	mgee_router:router({ClientSock, Module, Method, Data, AccountName, none, none}),
	    	do_client(AccountName, ClientSock);
		
		{error, timeout} -> 
			exit(timeout);
		
		{error, Reason} -> 
			exit(Reason)
    end.

do_client_after_enter(AccountName, Roleid, RoleName, ClientSock, Parent) ->
    case mgee_packet:recv(ClientSock) of
		{ok, heartbeat} ->
			Parent ! {event, hearbeat},
			do_client_after_enter(AccountName, Roleid, RoleName, ClientSock, Parent);
		{ok, {Module, Method, Data}} ->
	    	mgee_router:router({ClientSock, Module, Method, Data, AccountName, Roleid, RoleName}),
	    	do_client_after_enter(AccountName, Roleid, RoleName, ClientSock, Parent);
		
		{error, timeout} -> 
			exit(timeout);
		
		{error, Reason} -> 
			exit(Reason)
    end.

get_role(Roleid, Rolelist) ->
    lists:filter(fun(T) ->
			 if 
			     T#p_game_role.roleid =:= Roleid -> 
				 true;
			     true -> 
				 false
			 end
		 end, 
		 Rolelist).

clear_before_exit(State) ->
    if 
	is_pid(State#account_state.role_pid) ->
	    mgee_virtual_world_router:exit_vw(State#account_state.role_pid),
	    mod_team_server:role_exit_game(State#account_state.cur_roleid,
					   State#account_state.cur_rolename,  
					   State#account_state.role_pid ),
	    exit(State#account_state.role_pid, terminate);
	true ->
	    ok
    end,
    ok.
