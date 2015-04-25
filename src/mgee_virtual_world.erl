%%%----------------------------------------------------------------------
%%% File    : mgee_virtual_world.erl
%%% Author  : Qingliang
%%% Created : 2010-1-4
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_virtual_world).

-define(VW_SLICE_WIDTH, 7).
-define(VW_SLICE_HEIGHT, 9).

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
		 start_link/1,
		 broad_in_sence/3,
		 broad_in_sence_include/3
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(vw_state, {
				   vwid, 
				   pg2_name, 
				   tile_width, 
				   tile_height, 
				   tile_width_max_value, 
				   tile_height_max_value,
				   mod_fight_pid
				  }).

%% ====================================================================
%% External functions
%% ====================================================================

%% we can use the function to broadcast info to in sence role of RoleIdList
%% notice that RoleIdList self is not included.
-spec broad_in_sence(Pid :: pid() | atom, RoleIdList :: list(), Bin :: binary) -> no_return().
broad_in_sence(Pid, RoleIdList, Bin) when is_pid(Pid) or is_atom(Pid) ->
	case is_list(RoleIdList) and is_binary(Bin) of
		true ->
			gen_server:cast(
				Pid, 
				{broadcast_in_sence, RoleIdList, Bin});
		false ->
			?TEST_MSG("wrong params ~p ~p", [RoleIdList, Bin])
	end.

broad_in_sence_include(Pid, RoleIdList, Bin) when is_pid(Pid) or is_atom(Pid) ->
	case is_list(RoleIdList) and is_binary(Bin) of
		true ->
			gen_server:cast(
				Pid, 
				{broadcast_in_sence_include, RoleIdList, Bin});
		false ->
			?TEST_MSG("wrong params ~p ~p", [RoleIdList, Bin])
	end.
%% ====================================================================
%% Server functions
%% ====================================================================

start_link({ServerName, VMId}) ->
	gen_server:start_link({local, ServerName}, ?MODULE, VMId, ?GEN_SERVER_OPTIONS).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(VWIdInt) ->
	?INFO_MSG("~p init: ~p",[?MODULE, VWIdInt ]),
	?TEST_MSG("begin to init mgee_virtual_world_~p", [VWIdInt]),
	process_flag(trap_exit, true),	
	mgee_prof:setup_profiling(),
	VWId = integer_to_list(VWIdInt),
	%% read vw data
	case ets:lookup(?ETS_IN_VW_MAP_DATA, VWId) of
		[{VWId, {_, TileWidth, TileHeight, Data}}] ->
			process_flag(trap_exit, true),
			
			Name = mgee_tool:list_to_atom2(lists:concat(["pg2_virtual_world_client_list_", VWId])),
			pg2:create(Name),
			%% TODO start the fight mod, we should use hook to do this
			case mod_fight:start(VWId, self()) of
				{ok, Pid} ->
					link(Pid),
					%% use process dict to save the people in this virtual world
					lists:foreach(
						fun({Position, CanPass}) ->
							put(Position, CanPass)
						end,
					Data),
					slice_vw(TileWidth, TileHeight, VWId),
					X = mgee_tool:ceil(TileWidth/?VW_SLICE_WIDTH),
					Y = mgee_tool:ceil(TileHeight/?VW_SLICE_HEIGHT),
					{ok, #vw_state{
						   vwid=VWId, 
						   pg2_name=Name, 
						   tile_width = TileWidth,
						   tile_height = TileHeight,
						   tile_width_max_value = X - 1,
						   tile_height_max_value = Y - 1,
						   mod_fight_pid = Pid
						  }};
				_ ->
					{stop, can_not_start_mod_fight}
			end;
		[] ->
			{stop, can_not_read_vw_data}
	end.
    

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

handle_call({quit, Pid}, From, #vw_state{pg2_name=Pg2} = State) ->
	Reply =	case lists:member(Pid, pg2:get_members(Pg2)) of
		false -> From ! {error, not_in};
		%% not found , add it
		true ->  pg2:leave(Pg2, Pid),
	   		   From ! {ok, quited}
	end,
	{reply, Reply, State};
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

%% chat in virtual world, just send a message to every client in the current vw
%% because we don't need to reply the caller ,so we use the handle_cast function

handle_cast({quit, Pid, Roleid, Tx, Ty}, #vw_state{pg2_name=Pg2} = State) 
  when is_pid(Pid) and is_integer(Roleid) and is_integer(Tx) and is_integer(Ty) ->
	pg2:leave(Pg2, Pid),
	case get_role_slice(Roleid, State#vw_state.vwid) of
		{ok, Slice} ->
			pg2:leave(Slice, Pid),
			erase_role_vw_info(Roleid),
			AllSlice = get_9_slice_by_tile(
							 State#vw_state.vwid,
							 State#vw_state.tile_width_max_value, 
							 State#vw_state.tile_height_max_value, 
							 Tx, 
							 Ty),
			AllInSenceUser = lists:delete(Pid, get_all_in_sence_user_by_slice_list(AllSlice)),
			DataSend = mgee_packet:packet_encode(<<"vw">>, <<"quit">>, #m_vw_quit_toc{roleid=Roleid}),
			spawn(fun() -> broadcast(AllInSenceUser, DataSend) end);
		{error, _} ->
			none
	end,
	{noreply, State};

handle_cast({enter, _Module, _Method, Tx, Ty, Pid, Roleid}, #vw_state{pg2_name=Pg2} = State) ->
	?DEBUG("response enter virtual ~p: x:~p y:~p", [Pid, Tx, Ty]),
	case lists:member(Pid, pg2:get_members(Pg2)) of
			true -> 
				error;
			%% not found , add it
			false ->  
				AllSlice = get_9_slice_by_tile(
							 State#vw_state.vwid,
							 State#vw_state.tile_width_max_value, 
							 State#vw_state.tile_height_max_value, 
							 Tx, 
							 Ty),
				AllInSenceUser = lists:delete(Pid, get_all_in_sence_user_by_slice_list(AllSlice)),
				spawn(fun() -> enter(Pid, AllInSenceUser, true) end),
				%% to ensure that the client recv the enter packet first, not other packet like chat
				pg2:join(Pg2, Pid),
				update_slice_by_tile_first_enter(Roleid, Pid, State#vw_state.vwid, Tx, Ty)
	end,
	{noreply, State};

handle_cast({walk, ClientSock, Module, Method, DataIn, Roleid}, State) 
  when is_binary(Module) and is_binary(Method) and is_record(DataIn, m_move_walk_tos) and is_integer(Roleid) ->
	%% check whether can walk
	Tx = DataIn#m_move_walk_tos.tx,
	Ty = DataIn#m_move_walk_tos.ty,
	Px = DataIn#m_move_walk_tos.px,
	Py = DataIn#m_move_walk_tos.py,
	CanPass = get({Tx, Ty}),
	if 
		CanPass =:= true ->
			%% get user's cur slice
			%% update user's slice when needed
			case mgee_misc:get_role_pid(Roleid) of
				undefined ->
					ignore;
				Pid when is_pid(Pid) ->
					VwId = State#vw_state.vwid,
					case get_role_tx_ty(Roleid, VwId) of
						{ok, OldTx, OldTy} ->
							update_slice_by_tile(Roleid, Pid, State#vw_state.vwid, Tx, Ty),
							AllSlice = get_new_around_slice(
								 			State#vw_state.vwid,
								 			State#vw_state.tile_width_max_value, 
											State#vw_state.tile_height_max_value, 
								 			Tx, 
								 			Ty,
								 			OldTx,
								 			OldTy),
							AllInSenceUser = lists:delete(Pid, 
										lists:foldl(
											fun(Pg2Name, Acc) ->
												case pg2:get_members(Pg2Name) of
													{error, Reason} ->
														?TEST_MSG("pg2 getmembers fail: ~p", [Reason]),
														pg2:create(Pg2Name),
														Acc;
													T ->
														lists:merge(T, Acc)
												end
											end, [], AllSlice)),
							%% tell the others that "i'm in!"
							spawn(fun() -> enter(Pid, AllInSenceUser, false) end),
							%% get the new guys in sence
					
							lists:foreach(
								fun(P) ->
									case mgee_virtual_world_router:get_role_state_by_pid(P) of
										{ok, SelfStatus} ->
											DataRecord = #m_vw_enter_toc{return_self=false, game_role=SelfStatus#p_role_server_status.roleinfo},
											mgee_packet:packet_encode_send2(ClientSock, <<"vw">>, <<"enter">>, DataRecord);
										_Other ->
											ignore
									end
								end, AllInSenceUser),
							gen_server:cast(mgee_virtual_world_router, {walk, Roleid, Tx, Ty, Px, Py});
						{error, undefined} ->
							error
					end
				end;
		true ->
			DataRecord = #m_move_walk_toc{succ=false,tx=Tx,ty=Ty,px=Px,py=Py, reason = ?_LANG_WALK_CANNOT_PASS},
			mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRecord)
	end,
	{noreply, State};

handle_cast({walk_path, ClientSock, Module, Method, DataIn, Roleid}, State) 
  when is_record(DataIn, m_move_walk_path_tos) and is_integer(Roleid) ->
	%% don't need to check
	gen_server:cast(mgee_virtual_world_router, {walk_path, Roleid, DataIn}),
	%%DataRecord = #m_move_walk_toc{succ=false,tx=Tx,ty=Ty,px=Px,py=Py},
	DataRecord = #m_move_walk_path_toc{
									   bpx=DataIn#m_move_walk_path_tos.bpx,
										bpy=DataIn#m_move_walk_path_tos.bpy,
										epx=DataIn#m_move_walk_path_tos.epx,
										epy=DataIn#m_move_walk_path_tos.epy,
									   path=DataIn#m_move_walk_path_tos.path
									   },
	mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRecord),
	DataOther = #m_move_walk_path_toc{
									roleid=Roleid,
									bpx=DataIn#m_move_walk_path_tos.bpx,
									bpy=DataIn#m_move_walk_path_tos.bpy,
									epx=DataIn#m_move_walk_path_tos.epx,
									epy=DataIn#m_move_walk_path_tos.epy,
									return_self=false,
									path=DataIn#m_move_walk_path_tos.path
									},
	VwId = State#vw_state.vwid,
	case get_role_tx_ty(Roleid, VwId) of
		{ok, Tx, Ty}->
			AllSlice = get_9_slice_by_tile(
							 State#vw_state.vwid,
							 State#vw_state.tile_width_max_value, 
							 State#vw_state.tile_height_max_value, 
							 Tx, 
							 Ty),
			Pid = mgee_misc:get_role_pid(Roleid),
			AllInSenceUser = lists:delete(Pid, get_all_in_sence_user_by_slice_list(AllSlice)),
			DataOtherBin = mgee_packet:packet(Module, Method, mgee_packet:encode(Module, Method, DataOther)),
			broadcast(AllInSenceUser, DataOtherBin);
		{error, _} ->
			ignore
	end,
	{noreply, State};

handle_cast({vw_chat, Content}, #vw_state{pg2_name=Pg2} = State) ->
	lists:foreach(fun(ClientPid) -> ClientPid ! {vm_chat, Content} end, pg2:get_members(Pg2)),
	{noreply, State};

handle_cast({broadcast_in_sence, RoleIdList, DataBin}, State) 
  when is_list(RoleIdList) and is_binary(DataBin) ->
	VwId = State#vw_state.vwid,
	MaxTileWidthValue = State#vw_state.tile_width_max_value,
	MaxTileHeightValue = State#vw_state.tile_height_max_value,
	%% find all role's in_sence slice(9 gong ge)
	AllSlice = get_9_slice_by_roleid_list(VwId, MaxTileWidthValue, MaxTileHeightValue, RoleIdList),
	?TEST_MSG("AllSlice ~p", [AllSlice]),
	%% get all in sence roles
 	AllInSenceUser = get_all_in_sence_user_by_slice_list(AllSlice),
	%% remove them self
	?TEST_MSG("AllInSenceUser ~p", [AllInSenceUser]),
	AllInSenceUser2 = 
		lists:foldl(
			fun(Roleid, Acc) ->
				Pid = mgee_misc:get_role_pid(Roleid),
				lists:delete(Pid, Acc)
			end, AllInSenceUser, RoleIdList),
	?TEST_MSG("AllInSenceUser2 ~p", [AllInSenceUser2]),
	broadcast(AllInSenceUser2, DataBin),
	{noreply, State};


handle_cast({broadcast_in_sence_include, RoleIdList, DataBin}, State) ->
	VwId = State#vw_state.vwid,
	MaxTileWidthValue = State#vw_state.tile_width_max_value,
	MaxTileHeightValue = State#vw_state.tile_height_max_value,
	%% find all role's in_sence slice(9 gong ge)
	AllSlice = get_9_slice_by_roleid_list(VwId, MaxTileWidthValue, MaxTileHeightValue, RoleIdList),
	?TEST_MSG("AllSlice ~p", [AllSlice]),
	%% get all in sence roles
 	AllInSenceUser = get_all_in_sence_user_by_slice_list(AllSlice),
	%% remove them self
	?TEST_MSG("AllInSenceUser ~p", [AllInSenceUser]),
	broadcast(AllInSenceUser, DataBin),
	{noreply, State};


handle_cast({pos_sync, Roleid, Tx, Ty}, State) ->
	case get_role_vw_info(Roleid) of
		undefined ->
			ignore;
		{_, _, Slice} ->
			update_role_vw_info(Roleid, {Tx, Ty, Slice})
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
handle_info({'EXIT', Pid, Reason}, State) ->
	%% TODO use hook to do this
	?TEST_MSG("pid ~p exit: ~p", [Pid, Reason]),
	ModFightPid = State#vw_state.mod_fight_pid,
	if 
		Pid =:=	ModFightPid ->
			case mod_fight:start(State#vw_state.vwid, self()) of
				{ok, NewPid} ->
					link(NewPid),
					NewState = State#vw_state{mod_fight_pid=NewPid},
					{noreply, NewState};
				_ ->
					{stop, can_not_start_mod_fight, State}
			end;
		true ->
			?TEST_MSG("unknow pid ~p", [Pid]),
			{noreply, State}
	end;
	
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?INFO_MSG("vw terminate : ~p , reason: ~p", [Reason, State]),
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

broadcast(PidList, Data) ->
	lists:foreach(
		fun(Pid) -> 
			case mgee_misc:get_socket_by_rolepid(Pid) of
				{ok, ClientSocket} ->
					mgee_packet:send(ClientSocket, Data);
				_ ->
					none
			end
		end, PidList),
	ok.

enter(Pid, AllInSenceUser, RtnSelf) ->
	Module = <<"vw">>, 
	Method = <<"enter">>,
	case ets:lookup(mgee_tcp_client_info_list, Pid) of
		[{Pid, ClientSocket, RoleState}] -> 
			AllClient = lists:foldl(
							fun(CPid, ACC0) ->
								case mgee_virtual_world_router:get_role_state_by_pid(CPid) of
									{ok, RoleState2} ->
										[RoleState2|ACC0];
									_ ->
										ACC0
								end
							end, [], AllInSenceUser),
			if 
				RtnSelf =:= true ->
					DataRecord = #m_vw_enter_toc{result = #p_role_status_list{role_status=AllClient}},
					mgee_packet:packet_encode_send(ClientSocket, Module, Method, DataRecord);
				true ->
					none
			end,
			DataRecord2 = #m_vw_enter_toc{
										  return_self=false, 
										  roleid=(RoleState#p_role_server_status.roleinfo)#p_game_role.roleid,
										  game_role = (RoleState#p_role_server_status.roleinfo)
										 },
			DataOther = mgee_packet:packet(
						  Module, 
						  Method, 
						  mgee_packet:encode(Module, Method, DataRecord2)),
			broadcast(AllInSenceUser, DataOther);
		_ -> 
			error
	end.

slice_vw(TileWidth, TileHeight, VwId) ->
	X = mgee_tool:ceil(TileWidth/?VW_SLICE_WIDTH),
	Y = mgee_tool:ceil(TileHeight/?VW_SLICE_HEIGHT),
	lists:foreach(
		fun(SX) ->
			lists:foreach(
				fun(SY) -> 
					pg2:create(get_slice_name(VwId, SX, SY))
				end, lists:seq(0, Y-1))
		end, lists:seq(0, X-1)),
	%% mgee_virtual_world_slice
	ok.

get_slice_by_tile(VwId, X, Y) ->
	SX = mgee_tool:floor(X/?VW_SLICE_WIDTH),
	SY = mgee_tool:floor(Y/?VW_SLICE_HEIGHT),
	get_slice_name(VwId, SX, SY).

update_slice_by_tile_first_enter(Roleid, Pid, VwId, NewX, NewY) ->
	NewSlice = get_slice_by_tile(VwId, NewX, NewY),
	pg2:join(NewSlice, Pid),
	update_role_vw_info(Roleid, {NewX, NewY, NewSlice}).

update_slice_by_tile(Roleid, Pid, VwId, NewX, NewY) when is_pid(Pid) ->
	NewSlice = get_slice_by_tile(VwId, NewX, NewY),
	?TEST_MSG("NewSlice ~p", [NewSlice]),
	case get_role_slice(Roleid, VwId) of 
		{ok, OldSlice} ->
			?TEST_MSG("OldSlice ~p", [OldSlice]),
			if 
				NewSlice =/= OldSlice ->
					pg2:join(NewSlice, Pid),
					pg2:leave(OldSlice, Pid),
					update_role_vw_info(Roleid, {NewX, NewY, NewSlice});
				true ->
					none
			end;
		{error, Reason} ->
			?TEST_MSG("Reason ~p", [Reason]),
			pg2:join(NewSlice, Pid),
			update_role_vw_info(Roleid, {NewX, NewY, NewSlice})
	end;

update_slice_by_tile(_Roleid, _Pid, _VwId, _NewX, _NewY) ->
	ignore.

%% 9 gong ge
get_9_slice_by_tile(VwId, SliceWidthMaxValue, SliceHeightMaxValue, X, Y) ->
	SX = mgee_tool:floor(X/?VW_SLICE_WIDTH),
	SY = mgee_tool:floor(Y/?VW_SLICE_HEIGHT),
	if 
		SX > 0 ->
			BeginX = SX - 1;
		true ->
			BeginX = 0
	end,
	if
		SY > 0 ->
			BeginY = SY - 1;
		true ->
			BeginY = 0
	end,
	if 
		SX >= SliceWidthMaxValue ->
			EndX = SliceWidthMaxValue;
		true ->
			EndX = SX + 1
	end,
	if 
		SY >= SliceHeightMaxValue ->
			EndY = SliceHeightMaxValue;
		true ->
			EndY = SY + 1
	end,
	get_9_slice_by_tile_2(VwId, BeginX, BeginY, EndX, EndY).

get_9_slice_by_roleid_list(VwId, SliceWidthMaxValue, SliceHeightMaxValue, RoleIdList) ->
	lists:foldl(
		fun(Roleid, Acc) ->
			case get_role_tx_ty(Roleid, VwId) of
				{ok, AX, AY} ->
					Slices = get_9_slice_by_tile(VwId, SliceWidthMaxValue, SliceHeightMaxValue, AX, AY),
					mgee_tool:combine_lists(Acc, Slices);
				{error, _} ->
					Acc
			end
		end, [], RoleIdList).

get_9_slice_by_tile_2(VwId, BeginX, BeginY, EndX, EndY) ->
	lists:foldl(
		fun(TempSX, Acc) ->
			lists:foldl(
				fun(TempSY, AccSub) ->
					Temp = get_slice_name(VwId, TempSX, TempSY),
					[Temp|AccSub]
				end,
				Acc,
				lists:seq(BeginY, EndY)
			)
		end, [], lists:seq(BeginX, EndX)).
	
get_slice_name(VwId, SX, SY) -> 
	mgee_tool:list_to_atom2(lists:concat(["pg2_vw_slice_", VwId, "_", SX, "_", SY])).

get_all_in_sence_user_by_slice_list(SliceList) ->
	lists:foldl(
		fun(Pg2Name, Acc) ->
			case pg2:get_members(Pg2Name) of
				{error, Reason} ->
					?TEST_MSG("pg2 getmembers fail: ~p", [Reason]),
					pg2:create(Pg2Name),
					Acc;
				T ->
					lists:merge(T, Acc)
			end
		end, [], SliceList).
	
get_new_around_slice(VwId, SliceWidthMaxValue, SliceHeightMaxValue, NewSx, NewSy, OldSx, OldSy) ->
	TNew = get_9_slice_by_tile(VwId, SliceWidthMaxValue, SliceHeightMaxValue, NewSx, NewSy),
	TOld = get_9_slice_by_tile(VwId, SliceWidthMaxValue, SliceHeightMaxValue, OldSx, OldSy),
	lists:filter(
		fun(T) -> 
			case lists:member(T, TOld) of
				true ->
					false;
				false ->
					true
			end
		end, TNew).

get_role_vw_info(Roleid) ->
	get(Roleid).

get_role_tx_ty(Roleid, VwId) ->
	case get_role_vw_info(Roleid) of
		undefined -> 
			case mgee_virtual_world_router:get_role_state_by_roleid(Roleid) of
				{ok, RoleState} ->
					Tx = mgee_virtual_world_router:get_tx_by_state(RoleState),
					Ty = mgee_virtual_world_router:get_ty_by_state(RoleState),
					Slice = get_slice_by_tile(VwId, Tx, Ty),
					update_role_vw_info(Roleid, {Tx, Ty, Slice}),
					{ok, Tx, Ty};
				{error, _} ->
					{error, undefined}
			end;
		{Tx, Ty, _} ->
			{ok, Tx, Ty}
	end.

get_role_slice(Roleid, VwId) ->
	case get_role_vw_info(Roleid) of
		undefined -> 
			%% may be the vw restart for some reason
			case mgee_virtual_world_router:get_role_state_by_roleid(Roleid) of
				{ok, RoleState} ->
					Tx = mgee_virtual_world_router:get_tx_by_state(RoleState),
					Ty = mgee_virtual_world_router:get_ty_by_state(RoleState),
					Slice = get_slice_by_tile(VwId, Tx, Ty),
					update_role_vw_info(Roleid, {Tx, Ty, Slice}),
					{ok, Slice};
				{error, _} ->
					{error, undefined}
			end;
		{_, _, Slice} ->
			{ok, Slice}
	end.
			

%% Info => {Tx, Ty, Slice}
update_role_vw_info(Roleid, Info) ->
	put(Roleid, Info).

erase_role_vw_info(Roleid) ->
	erase(Roleid).