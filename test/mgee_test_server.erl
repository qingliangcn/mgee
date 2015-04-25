%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-14
%%% @doc simulate stress test
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_test_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0]).

-export([
		 get_info/0,
		 test_start/0,
		 test_start/1,
		 test_start/2,
		 test_stop/0
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

get_info() ->
	[
		ets:info(ets_pid), 
		ets:tab2list(ets_pid)
    ].

test_start() ->
	{ok, ProcessCount} = application:get_env(process_count),
	test_start(ProcessCount, test_chat).

% @doc start a test, simulate count read from mgee_test_app.app file
test_start(TestTarget) ->
	{ok, ProcessCount} = application:get_env(process_count),
	test_start(ProcessCount, TestTarget).

% @doc start a test, at ProcessCount simulate number.
test_start(ProcessCount, TestTarget) ->
	?DEBUG("~p", [TestTarget]),
	gen_server:cast(?MODULE, {test_start, ProcessCount, TestTarget}).

test_stop() ->
	gen_server:cast(?MODULE, test_stop ).


start() ->
	{ok, _} = supervisor:start_child(
				mgee_test_sup, 
				{mgee_test_server_sup,
				{mgee_test_server_sup, start_link, []},
				transient, infinity, supervisor, [mgee_test_server_sup]}),
	{ok, _} = supervisor:start_child(
				mgee_test_sup, 
				{mgee_test_server,
				{mgee_test_server, start_link, []},
				transient, brutal_kill, worker, [mgee_test_server]}),
	ok.

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
	ets:new( ets_pid,  [set, protected, named_table]),
    {ok, []}.

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

handle_cast({test_start, ProcessCount, TestTarget}, State) ->
	?DEBUG("~p", [TestTarget]),
	{ok, CHost} = application:get_env(host),
	{ok, CPort} = application:get_env(port),
	{ok, CTimeout} = application:get_env(timeout),
	{ok, CSpeed} = application:get_env(speed),
	{ok, CStartNumber} = application:get_env(process_start_number),
	PidList = start_worker(CStartNumber, CStartNumber + ProcessCount - 1, {CHost, CPort, CTimeout, CSpeed}, TestTarget),
	?DEBUG("test_start count: ~p, PidList: ~p", [ProcessCount, PidList]),
	
	ets:insert( ets_pid, {all_pids, PidList}),	
	
	
    {noreply, State}
	;

handle_cast( test_stop, State) ->
    {noreply, State}
	;

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

start_worker(CStartNumber, NN, _Config, _TestTarget) when NN < CStartNumber ->
	[];
start_worker(CStartNumber, N, Config, TestTarget) ->
	?DEBUG("~p", [TestTarget]),
	Pid = start_worker_run(N, Config),
	gen_server:cast(Pid, TestTarget ),
	timer:sleep(100),
	PidList = start_worker(CStartNumber, N-1, Config, TestTarget),
	[Pid | PidList].

start_worker_run(N, Config) ->
	{ok, Pid} = supervisor:start_child(mgee_test_server_sup, [N, Config]),
	Pid.
	
