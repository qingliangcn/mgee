%%% -------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author Qingliang, 2010-01-12
%%% @doc the flash cross domain policy
%%% @end
%%% -------------------------------------------------------------------
-module(mgee_cross_domain).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("mgee.hrl").

-define(CROSS_DOMAIN_TCP_OPTIONS, [
								  binary, 
								  {packet, 0}, % no packaging 
								  {reuseaddr, true}, % allow rebind without waiting 
								  {active, false},
								  {exit_on_close, false}
								 ]).


%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, send_file/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, mgee_cross_domain},?MODULE, [], []).

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
	start_server().

send_file(CSock) ->
	case gen_tcp:recv(CSock, 0) of
		{ok, ?CROSS_DOMAIN_FLAG} -> Data = list_to_binary(?CROSS_FILE),
											  gen_tcp:send(CSock, Data);
		_-> error
	end,
	gen_tcp:close(CSock).
    
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
handle_info({'EXIT', _Pid, normal}, State) ->
	{stop, normal, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
	?ERROR_MSG("mgee_cross_domain loop exit with reason:~p", [Reason]),
	case erlang:is_port(State) of
		true -> gen_tcp:close(State)
	end,
	case start_server() of
		{ok, LSock} -> {noreply, LSock};
		{error, Reason} -> {stop, Reason, State}
	end;
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

start_server() ->
	case gen_tcp:listen(843, ?CROSS_DOMAIN_TCP_OPTIONS) of
		{ok, LSock} -> spawn_link(fun() -> loop(LSock) end),
					   ?DEBUG("mgee_cross_domain started on port 843", []),
					   {ok, LSock};
		%% @todo throw exception here and we must do this at our work port 
		{error, Reason} -> ?ERROR_MSG("can't listen on port 843 with reason:~p", [Reason]),
						   {stop, Reason}
	end.

loop(LSock) ->
	case gen_tcp:accept(LSock) of
		{ok, CSock} -> spawn(fun() -> send_file(CSock) end);
		{error, Reason} -> Reason
	end,
	loop(LSock).