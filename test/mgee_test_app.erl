%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-14
%%% @doc simulate stress test application
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_test_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1,
	 start/0,
	 stop/0
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(APPS, [sasl, mgee_test_app]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

start() ->
	try
        ok = mgee_misc:start_applications(?APPS) 
    after
        %%give the error loggers some time to catch up
        timer:sleep(100)
    end.

stop() ->
    ok = mgee_misc:stop_applications(?APPS).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Name, []) ->
	{ok, SupPid} = mgee_test_sup:start_link(),
	lists:foreach(
      fun ({Msg, Thunk}) ->
              io:format("starting ~-40s ...", [Msg]),
              Thunk(),
              io:format("done~n");
          ({Msg, M, F, A}) ->
              io:format("starting ~-40s ...", [Msg]),
              apply(M, F, A),
              io:format("done~n")
      end,
	  [{"MGEE Logger",
	    fun() ->
			{ok, LogLevel} = application:get_env(log_level),
			mgee_loglevel:set(LogLevel),
			{ok, LogPath} = application:get_env(log_path),
			error_logger:add_report_handler(mgee_logger_h, LogPath)
		end},
	   {"Mgee Test Server",
		fun() ->
				mgee_test_server:start()
		end}
	  ]
	  ),
	io:format("~nmgee_test_app running.~n"),
	{ok, TestTarget} = application:get_env(test_target),
	mgee_test_server:test_start(TestTarget),
    {ok, SupPid}.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

