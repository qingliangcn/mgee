%%%----------------------------------------------------------------------
%%% File    : mgee_tcp_listener_sup.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_tcp_listener_sup).

-behaviour(supervisor).

-include("mgee.hrl").

-export([start_link/7, start_link/8]).

-export([init/1]).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
           AcceptCallback, Label) ->
    start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
               AcceptCallback, 1, Label).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
           AcceptCallback, ConcurrentAcceptorCount, Label) ->
    supervisor:start_link(
      ?MODULE, {IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
                AcceptCallback, ConcurrentAcceptorCount, Label}).

init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
      AcceptCallback, ConcurrentAcceptorCount, Label}) ->

	?INFO_MSG("~p init: ~p",[?MODULE, {IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
		      AcceptCallback, ConcurrentAcceptorCount, Label} ]),

    %% This is gross. The tcp_listener needs to know about the
    %% mgee_tcp_acceptor_sup, and the only way I can think of accomplishing
    %% that without jumping through hoops is to register the
    %% mgee_tcp_acceptor_sup.
    Name = mgee_misc:tcp_name(mgee_tcp_acceptor_sup, IPAddress, Port),
    {ok, {{one_for_all, 10, 10},
          [{mgee_tcp_acceptor_sup, {mgee_tcp_acceptor_sup, start_link,
                               [Name, AcceptCallback]},
            transient, infinity, supervisor, [mgee_tcp_acceptor_sup]},
           {mgee_tcp_listener, {mgee_tcp_listener, start_link,
                           [IPAddress, Port, SocketOpts,
                            ConcurrentAcceptorCount, Name,
                            OnStartup, OnShutdown, Label]},
            transient, 100, worker, [mgee_tcp_listener]}]}}.
