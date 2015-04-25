%%%----------------------------------------------------------------------
%%% File    : mgee_virtual_world_ets_heir.erl
%%% Author  : Qingliang
%%% Created : 2010-2-8
%%% Description: heir of the vw_router's ets table
%%%----------------------------------------------------------------------
-module(mgee_virtual_world_ets_heir).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
	process_flag(trap_exit, true),
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
handle_call({get_back_ets, FromPid}, From, State) ->
	?TEST_MSG("~p want ets ~p", [From, FromPid]),
	case lists:member(?ETS_VW_LIST, ets:all()) of
		true ->
			ets:give_away(?ETS_VW_LIST, FromPid, from_vw_ets_heir),
			ets:give_away(?ETS_IN_VW_ROLE_LIST, FromPid, from_vw_ets_heir),
			ets:give_away(?ETS_IN_VW_MAP_DATA, FromPid, from_vw_ets_heir),
			{reply, ok, State};
		false ->
			{reply, error, State}
	end;
	
handle_call(Request, From, State) ->
	?TEST_MSG("unexpected request ~p from ~p", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?TEST_MSG("unexpected msg ~p", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'ETS-TRANSFER', Tid, FromPid, vw_router} , State) ->
	?TEST_MSG("vw_router ~p terminated, we recv the ets :~p", [FromPid, Tid]),
	{noreply, State};

handle_info(Info, State) ->
	?TEST_MSG("unexpected info ~p", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?TEST_MSG("~p teminate: ~p, State:~p", [?MODULE, Reason, State]),
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

