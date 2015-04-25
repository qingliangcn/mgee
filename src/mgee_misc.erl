%%%----------------------------------------------------------------------
%%% File    : mgee_misc.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_misc).

%%
%% Include files
%%
-include("mgee.hrl").
-include("game_pb.hrl").
%%
%% Exported Functions
%%
-export([manage_applications/6, start_applications/1, stop_applications/1, tcp_name/3]).

-export([
		 account_process_name/1,
		 role_process_name/1, 
		 whereis_name/1, 
		 register/3, 
		 get_account_pid/1, 
		 get_role_pid/1,
		 get_socket_by_roleid/1,
		 get_socket_by_rolepid/1,
		 get_vwid_by_roleid/1
		]).

%%
%% API Functions
%%

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).

tcp_name(Prefix, IPAddress, Port)
  when is_atom(Prefix) andalso is_number(Port) ->
    mgee_tool:list_to_atom2(
      lists:flatten(
        io_lib:format("~w_~s:~w",
                      [Prefix, inet_parse:ntoa(IPAddress), Port]))).

%% don't care about chinese, it performance well.
account_process_name(AccountName) when is_integer(AccountName) or is_atom(AccountName) ->
	mgee_tool:list_to_atom2(
	  lists:concat([mgee_account_, AccountName]));
account_process_name(AccountName) when is_list(AccountName) ->
	mgee_tool:list_to_atom2(
	  lists:flatten(["mgee_account_"|AccountName]));
account_process_name(AccountName) when is_binary(AccountName) ->
	mgee_tool:list_to_atom2(
		lists:concat([mgee_account_,mgee_misc:md5(AccountName)])).

role_process_name(Roleid) when is_integer(Roleid) or is_atom(Roleid) ->
	mgee_tool:list_to_atom2(
	  lists:concat([mgee_role_, Roleid]));
role_process_name(Roleid) when is_list(Roleid) ->
	mgee_tool:list_to_atom2(
	  lists:flatten(["mgee_role_"|Roleid]));
role_process_name(Roleid) when is_binary(Roleid) ->
	mgee_tool:list_to_atom2(
		lists:concat([mgee_role_,mgee_tool:md5(Roleid)])).

get_account_pid(AccountName) ->
	mgee_misc:whereis_name({local, mgee_misc:account_process_name(AccountName)}).

get_role_pid(Roleid)  ->
	 mgee_misc:whereis_name({local, mgee_misc:role_process_name(Roleid)}).

get_socket_by_roleid(Roleid) ->
	Pid = mgee_misc:get_role_pid(Roleid),
	case ets:lookup(?ETS_IN_VW_ROLE_LIST, Pid) of
		[{Pid, ClientSock, _}] ->
			{ok, ClientSock};
		_ ->
			{error, not_found}
	end.

get_socket_by_rolepid(Pid) ->
	case ets:lookup(?ETS_IN_VW_ROLE_LIST, Pid) of
		[{Pid, ClientSock, _}] ->
			{ok, ClientSock};
		_ ->
			{error, not_found}
	end.

get_vwid_by_roleid(Roleid) ->
	Pid = mgee_misc:get_role_pid(Roleid),
	case ets:lookup(?ETS_IN_VW_ROLE_LIST, Pid) of
		[{Pid, _, RoleInfo}] ->
			{ok, RoleInfo#p_game_role.vwid};
		_ ->
			{error, not_found}
	end.

%% get the pid of a registered name
whereis_name({local, Atom}) -> 
	erlang:whereis(Atom);

whereis_name({global, Atom}) ->
	global:whereis_name(Atom).

register(local, Name, Pid) ->
	erlang:register(Name, Pid);
register(global, Name, Pid) ->
	global:register_name(Name, Pid).
