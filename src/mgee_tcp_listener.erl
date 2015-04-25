%%%----------------------------------------------------------------------
%%% File    : mgee_tcp_listener.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_tcp_listener).

-behaviour(gen_server).

-include("mgee.hrl").

-export([start_link/8]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, on_startup, on_shutdown, label}).

%%--------------------------------------------------------------------

start_link(IPAddress, Port, SocketOpts,
           ConcurrentAcceptorCount, AcceptorSup,
           OnStartup, OnShutdown, Label) ->
    gen_server:start_link(
      ?MODULE, {IPAddress, Port, SocketOpts,
                ConcurrentAcceptorCount, AcceptorSup,
                OnStartup, OnShutdown, Label}, []).

%%--------------------------------------------------------------------

init({IPAddress, Port, SocketOpts,
      ConcurrentAcceptorCount, AcceptorSup,
      {M,F,A} = OnStartup, OnShutdown, Label}) ->

	?INFO_MSG("~p init: ~p",[?MODULE, {IPAddress, Port, SocketOpts,
      			ConcurrentAcceptorCount, AcceptorSup,
		      	{M,F,A} = OnStartup, OnShutdown, Label} ]),
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, SocketOpts ++ [{active, false}]) of
        {ok, LSock} ->
		%% if listen successful ,we start several acceptor to accept it
            lists:foreach(fun (_) ->
                                  	{ok, APid} = supervisor:start_child(
                                                  	AcceptorSup, [LSock]),
									APid ! {event, start}
                          end,
                          lists:duplicate(ConcurrentAcceptorCount, dummy)),
            {ok, {LIPAddress, LPort}} = inet:sockname(LSock),
            ?TEST_MSG("started ~s on ~s:~p~n",
                                  [Label, inet_parse:ntoa(LIPAddress), LPort]),
            apply(M, F, A ++ [IPAddress, Port]),
            {ok, #state{sock = LSock,
                        on_startup = OnStartup, on_shutdown = OnShutdown, 
                        label = Label}};
        {error, Reason} ->
            error_logger:error_msg(
              "failed to start ~s on ~s:~p - ~p~n",
              [Label, inet_parse:ntoa(IPAddress), Port, Reason]),
            {stop, {cannot_listen, IPAddress, Port, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock, on_shutdown = {M,F,A}, label=Label}) ->
    {ok, {IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    error_logger:info_msg("stopped ~s on ~s:~p~n",
                          [Label, inet_parse:ntoa(IPAddress), Port]),
    apply(M, F, A ++ [IPAddress, Port]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
