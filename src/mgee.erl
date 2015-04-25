%%%----------------------------------------------------------------------
%%% File    : mgee.erl
%%% Author  : Qingliang
%%% Purpose : MGEE application
%%% Created : 2010-01-01
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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
-define(APPS, [sasl, mgee]).

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
	{ok, SupPid} = mgee_sup:start_link(),
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
			mgee_loglevel:set(LogLevel)			
			% DO NOT ADD ERROR HANDLER! It will write log file twice.
			%{ok, LogPath} = application:get_env(log_path),
			%error_logger:add_report_handler(mgee_logger_h, LogPath)
		end},
	   {"PG2 Manager",
		fun() ->
				%% TODO: add a gen_server to manage pg2 groups.
				pg2:create(pg2_all_client),
				pg2:create(pg2_all_role)
		end},
	   {"Timer Server",
		fun() ->
				mgee_timer:start()
		end},
	   {"Mnesia table",
		fun () ->
			mgee_mnesia:init()
		end},
	   {"Mgee Persistent",
		fun () ->
			mgee_persistent_sup:start()
		end},
	   {"Mgee Router Server",
		fun() ->
			mgee_router:start()
		end
		},
	   {"Mgee Virtual World Router",
		fun() ->
				mgee_virtual_world_router:start()
		end},
	   {"Mgee Account Server",
		fun () ->
			mgee_account_server:start()
		end},
	   {"Mgee Team Server Module",
		fun () ->
			mod_team_server:start()
		end},
	   {"Mgee Chat Server",
		fun() ->
			mgee_chat_sup:start()	
		end},
	   {"Mod Skill",
		fun () ->
			{ok, SkillFilePath} = application:get_env(skill_file_path),
			mod_skill:start(SkillFilePath)
		end},
	   {"TCP listeners",
        fun () ->
                ok = mgee_networking:start(),
				{ok, AcceptorNum} = application:get_env(acceptor_num),
                {ok, TcpListeners} = application:get_env(tcp_listeners),
                lists:foreach(
                  fun ({Host, Port}) ->
                          ok = mgee_networking:start_tcp_listener(Host, Port, AcceptorNum)
                  end,
                  TcpListeners)
        end},
	   {"Test Data Setup",
		fun() ->
			ok
		end
		}
	  ]
	  ),
	io:format("~nsystem running :)~n"),
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

