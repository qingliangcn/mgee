%%%----------------------------------------------------------------------
%%% File    : mod_equip.erl
%%% Author  : Qingliang
%%% Created : 2010-2-8
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mod_equip).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([
		 handle/1
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
handle({ClientSock, Module, Method, Data, AccountName, Roleid, _RoleName}) 
  when is_record(Data, m_equip_change_tos) ->
	Display = #p_game_role_disply{type=Data#m_equip_change_tos.equip_slot_num, value=Data#m_equip_change_tos.equipid},
	case gen_server:call(mgee_account_server, {update_display, AccountName, Roleid, Display}) of
		ok ->
			DataRecordSelf = #m_equip_change_toc{},
			mgee_packet:packet_encode_send2(ClientSock, Module, Method, DataRecordSelf),
			DataRecordOther = #m_equip_change_toc{
										  return_self=false, 
										  roleid=Roleid,
										  equip_slot_num=Data#m_equip_change_tos.equip_slot_num,
										  equipid=Data#m_equip_change_tos.equipid
										  },
			case mgee_virtual_world_router:get_vwid_by_roleid(Roleid) of
				{error, _Reason} ->
					ignore;
				VwId ->
					VwName = mgee_virtual_world_router:get_virtual_world_name(VwId),
					Pid = mgee_misc:whereis_name({local, VwName}),
 					mgee_virtual_world:broad_in_sence(Pid, [Roleid], mgee_packet:packet_encode(Module, Method, DataRecordOther))
			end;
		error ->
			ignore
	end.

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

