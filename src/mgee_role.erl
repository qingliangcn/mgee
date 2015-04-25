%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-13
%%% @doc TODO: Add description to mod_role
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_role).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
		 start/3,
		 get_role_attr/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

start(Roleid, RoleBaseInfo, RoleAttr) ->
	gen_server:start(?MODULE, [Roleid, RoleBaseInfo, RoleAttr], ?GEN_SERVER_OPTIONS).

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
init([Roleid, RoleBaseInfo, RoleAttr]) ->
	process_flag(trap_exit, true),
	?INFO_MSG("~p init: ~p",[?MODULE, [Roleid, RoleBaseInfo, RoleAttr] ]),
	%todo, read role data from mnesia.
	RegName = mgee_misc:role_process_name(Roleid),
	case mgee_misc:whereis_name({local, RegName}) of
		Pid when is_pid(Pid) ->
			?ERROR_MSG("init role ~p failed, the role already exists", [Roleid]),
			{stop, already_exist};
		_ ->
			register(mgee_misc:role_process_name(Roleid), self()),
			{ok, #role_state{
					 roleid = Roleid, 
					 game_role = RoleBaseInfo, 
					 game_role_attr=RoleAttr }}
	end.

get_role_attr(Roleid) ->
	gen_server:call(mgee_misc:get_role_pid(Roleid), {get_attr}).

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

handle_call({get_attr}, _From, State) ->
	{reply, State#role_state.game_role_attr, State};

handle_call({role_state}, _From, State) ->
	{reply, State#role_state.game_role, State};

handle_call(Request, From, State) ->
	?TEST_MSG("unexpected call msg ~p from ~p", [Request, From]),
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

handle_info({'EXIT', Pid, terminate}, State) ->
	?DEBUG("process ~p send me terminate signal.", [Pid]),
    {stop, normal, State};

handle_info(Info, State) ->
	?DEBUG("receive unexpected msg ~p", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?INFO_MSG("mgee_role_~p terminated with reason: ~p", [State#role_state.roleid, Reason]),
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

