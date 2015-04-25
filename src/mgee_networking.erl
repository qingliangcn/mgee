%%%----------------------------------------------------------------------
%%% File    : mgee_networking.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_networking).

-define(MGEE_TCP_OPTS, [
        binary, 
        {packet, 0}, % no packaging 
        {reuseaddr, true}, % allow rebind without waiting 
        {nodelay, false},
        {delay_send, true}, 
		{active, false},
        {exit_on_close, false}
    ]).

%%
%% Include files
%%
-include("mgee.hrl").
-include_lib("kernel/include/inet.hrl").

%%
%% Exported Functions
%%
-export([start/0, check_tcp_listener_address/3, start_tcp_listener/3, start_listener/5, stop_tcp_listener/2]).

-export([tcp_listener_started/2, tcp_listener_stopped/2, tcp_host/1]).

%%
%% API Functions
%%

start() ->
	{ok, _} = supervisor:start_child(mgee_sup, {mgee_cross_domain,
                {mgee_cross_domain, start_link,[]},
                transient, brutal_kill, worker, [mgee_cross_domain]}),
    ok.

%%
%% Local Functions
%%

check_tcp_listener_address(NamePrefix, Host, Port) ->
    IPAddress =
        case inet:getaddr(Host, inet) of
            {ok, IPAddress1} -> IPAddress1;
            {error, Reason} ->
                ?ERROR_MSG("invalid host ~p - ~p~n",
                                       [Host, Reason]),
                throw({error, {invalid_host, Host, Reason}})
        end,
    if is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) -> ok;
       true -> ?ERROR_MSG("invalid port ~p - not 0..65535~n",
                                      [Port]),
               throw({error, {invalid_port, Port}})
    end,
    Name = mgee_misc:tcp_name(NamePrefix, IPAddress, Port),
    {IPAddress, Name}.


start_tcp_listener(Host, Port, AcceptorNum) ->
    start_listener(Host, Port, "TCP Listener", AcceptorNum,
                   {?MODULE, start_client, []}).

start_listener(Host, Port, Label, AcceptorNum, OnConnect) ->
    {IPAddress, Name} =
        check_tcp_listener_address(mgee_tcp_listener_sup, Host, Port),
    {ok,_} = supervisor:start_child(
               mgee_sup,
               {Name,
                {mgee_tcp_listener_sup, start_link,
                 [IPAddress, Port, ?MGEE_TCP_OPTS ,
                  {?MODULE, tcp_listener_started, []},
                  {?MODULE, tcp_listener_stopped, []},
                  OnConnect, AcceptorNum, Label]},
                transient, infinity, supervisor, [mgee_tcp_listener_sup]}),
    ok.

stop_tcp_listener(Host, Port) ->
    {ok, IPAddress} = inet:getaddr(Host, inet),
    Name = mgee_misc:tcp_name(mgee_tcp_listener_sup, IPAddress, Port),
    ok = supervisor:terminate_child(mgee_sup, Name),
    ok = supervisor:delete_child(mgee_sup, Name),
    ok.

tcp_listener_started(IPAddress, Port) ->
	?DEBUG("tcp_listener_started ~p ~p", [IPAddress, Port]),
%%     ok = mnesia:dirty_write(
%%            mgee_tcp_listener,
%%            #listener{node = node(),
%%                      protocol = tcp,
%%                      host = tcp_host(IPAddress),
%%                      port = Port}).
	ok.

tcp_listener_stopped(IPAddress, Port) ->
	?DEBUG("tcp_listener_started ~p ~p", [IPAddress, Port]),
%%     ok = mnesia:dirty_delete_object(
%%            mgee_tcp_listener,
%%            #listener{node = node(),
%%                      protocol = tcp,
%%                      host = tcp_host(IPAddress),
%%                      port = Port}).
	ok.

tcp_host({0,0,0,0}) ->
    {ok, Hostname} = inet:gethostname(),
    case inet:gethostbyname(Hostname) of
        {ok, #hostent{h_name = Name}} -> Name;
        {error, _Reason} -> Hostname
    end;
tcp_host(IPAddress) ->
    case inet:gethostbyaddr(IPAddress) of
        {ok, #hostent{h_name = Name}} -> Name;
        {error, _Reason} -> inet_parse:ntoa(IPAddress)
    end.