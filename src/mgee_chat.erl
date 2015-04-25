%%%----------------------------------------------------------------------
%%%
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-01-13
%%% @doc the mgee chat module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(mgee_chat).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
-include("global_lang.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([handle/1]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% microseconds
-define(CHAT_INTERVAL, 3000000).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).

handle({ClientSock, Module, Method, Data, AccountName, Roleid, RoleName}) ->
	case get(previous_chat_time) of
		undefined -> 
			?TEST_MSG("role ~p first chat", [RoleName]),
			PreviousChatTime = {0,0,0};
		Val ->
			PreviousChatTime = Val
	end,
	Now = mgee_timer:now(),
	?TEST_MSG("last chat time ~p, this chat time ~p", [PreviousChatTime, Now]),
	TimeDiff = timer:now_diff(Now, PreviousChatTime),
	?TEST_MSG("timer dif ~p", [TimeDiff]),
	if
		TimeDiff >= ?CHAT_INTERVAL ->
			put(previous_chat_time, Now),
			handle2({ClientSock, Module, Method, Data, AccountName, Roleid, RoleName});
		true ->
			case Method of
				<<"world">> ->
					#m_chat_world_toc{succ=false, reason = ?_LANG_CHAT_TOO_FAST};
				<<"bubble">> ->
					#m_chat_bubble_toc{succ=false, reason = ?_LANG_CHAT_TOO_FAST};
				_Other ->
					ignore
			end
	end.

handle2({ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName}) ->
	case Method of
		<<"world">> ->
			world(ClientSock, Module, Method, Data, Roleid, RoleName);
		<<"private">> ->
			private(ClientSock, Module, Method, Data, Roleid, RoleName);
		<<"vw">> ->
			vw(ClientSock, Module, Method, Data, Roleid, RoleName);
		<<"family">> ->
			family(ClientSock, Module, Method, Data, Roleid, RoleName);
		<<"team">> ->
			team(ClientSock, Module, Method, Data, Roleid, RoleName);
		<<"bubble">> ->
			bubble(ClientSock, Module, Method, Data, Roleid, RoleName);
		Other ->
			?DEBUG("undefined method ~p", [Other])
  	end,
	ok.

world(_ClientSock, Module, Method, Data, Roleid, RoleName) ->
	DataBroadcast = #m_chat_world_toc{
					body=Data#m_chat_world_tos.body,
					return_self=false,
					from_roleid=Roleid,
					from_rolename=RoleName
					},
	BinBroadcast = mgee_packet:packet_encode(Module, Method, DataBroadcast),
	cast({world, BinBroadcast}),
	ok.

private(ClientSock, Module, Method, Data, Roleid, RoleName) ->
	case mgee_misc:get_socket_by_roleid(Data#m_chat_private_tos.to_roleid) of
  		{ok, ToClientSock} ->
			DataRecord = #m_chat_private_toc{
									 body = Data#m_chat_private_tos.body,
									 from_roleid = Roleid,
									 from_rolename = RoleName,
									 return_self = false
									}, 
			mgee_packet:packet_encode_send(ToClientSock, Module, Method, DataRecord);
		Wrong ->
			DataRecord = #m_chat_private_toc{succ=false, reason= <<"用户不在线！">>},
			mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRecord),
			?DEBUG("find pid ~p socket failed", Wrong)
	end,
	ok.
	
bubble(_ClientSock, Module, Method, Data, Roleid, RoleName) ->
	VwId = mgee_virtual_world_router:get_vwid_by_roleid(Roleid),
	VwName = mgee_virtual_world_router:get_virtual_world_name(VwId),
	DataRecord = #m_chat_bubble_toc{
					from_roleid=Roleid, 
					from_rolename=RoleName, 
					body=Data#m_chat_bubble_tos.body,
					return_self=false
					},
	DataBin = mgee_packet:packet_encode(Module, Method, DataRecord),
	mgee_virtual_world:broad_in_sence_include(VwName, [Roleid], DataBin).

% send msg to virual world( map )
vw(_ClientSock, _Module, _Method, _Data, _Roleid, _RoleName) ->
	ok.
team(_ClientSock, _Module, _Method, _Data, _Roleid, _RoleName) ->
	ok.
family(_ClientSock, _Module, _Method, _Data, _Roleid, _RoleName) ->
	ok.
	
cast(Msg) ->
	gen_server:cast(mgee_chat_sup:get_chat_name(erlang:system_info(scheduler_id)), Msg).

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
    {ok, #state{}}.

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

handle_cast({world, BinBroadcast}, State) ->
	lists:foreach(
		fun(Pid) ->
			case mgee_misc:get_socket_by_rolepid(Pid) of
				{ok, ToClientSock} ->
					mgee_packet:send(ToClientSock, BinBroadcast);
				Wrong ->
					?DEBUG("find pid ~p socket failed", [Wrong])
			end
		end,
		pg2:get_members(pg2_all_role)),
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

