%%%----------------------------------------------------------------------
%%% File    : mgee_logger_h.erl
%%% Author  : Qingliang
%%% Created : 2010-01-01
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-define(VERSION, element(2, application:get_key(mgee,vsn))).

%% -define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
%% -define(MYNAME, hd(ejabberd_config:get_global_option(hosts))).
%% -define(MYLANG, ejabberd_config:get_global_option(language)).

-ifdef(DEBUG).
-define(GEN_SERVER_OPTIONS, [{debug, [trace,log]}]).
-else.
-define(GEN_SERVER_OPTIONS, []).
-endif.

-define(MSGS_DIR, "msgs").
-define(CONFIG_PATH, "mgee.cfg").
-define(LOG_PATH, "mgee.log").

-define(CROSS_FILE, "<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM "
	   ++"\"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">\n"
	   ++"<cross-domain-policy>\n"
    ++"<allow-access-from domain=\"*\" to-ports=\"80,8888\"/>\n"
    ++"</cross-domain-policy>\n\0").

%% equal to <<"<policy-file-request/>\0">>
-define(CROSS_DOMAIN_FLAG, <<60,112,111,108,105,99,121,45,102,105,108,101,45,114,101,113,117,101,115,116,47,62,0>>).

%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(TEST_MSG(Format, Args),
    mgee_logger:test_msg(?MODULE,?LINE,Format, Args)).

-define(DEBUG(Format, Args),
    mgee_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    mgee_logger:info_msg(?MODULE,?LINE,Format, Args)).
			      
-define(WARNING_MSG(Format, Args),
    mgee_logger:warning_msg(?MODULE,?LINE,Format, Args)).
			      
-define(ERROR_MSG(Format, Args),
    mgee_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    mgee_logger:critical_msg(?MODULE,?LINE,Format, Args)).

-record(listener, {node, protocol, host, port}).

-record(client_state, {socket, userid, username, sex, skin, x, y, lastPath, speed=1, vwid=0}).

% account data
-record(account_state, {
						client_sock, 
						hand_shaking_data, 
						account_name, 
						money_gold, 
						ip, 
						cur_roleid, 
						cur_rolename,
						do_client_pid,
						role_pid,
						last_heartbeat_time,
						heartbeat_fail_num = 0
					   }
	   ).

% rolelist is a list = [ game_role,  game_role, game_role, ...]
-record(game_account, {account_name, rolelist }).

% role data
-record(role_state, {roleid, game_role, game_role_attr, game_role_bag, game_role_team}).

-record(game_role_bag, {roleid, itemlist}).

-record(game_role_team, {roleid, teamid }).

-record(game_team, {teamid, teamname, memberlist }).

-define(ACCOUNT_ROLE_COUNT_MAX, 3).

-define(RECV_TIMEOUT, 5000).

-define(LOGIN_MODULE, <<"login">>).

-define(ETS_IN_VW_ROLE_LIST, mgee_tcp_client_info_list).
-define(ETS_IN_VW_MAP_DATA, mgee_ets_in_vw_map_data).
-define(ETS_VW_LIST, mgee_virtual_world_list).


-define(TEAM_MAX_ROLE_COUNT, 5).

-define(HEARTBEAT_TICKET_TIME, 3000).

-define(HEARTBEAT_MAX_FAIL_TIME, 1).
