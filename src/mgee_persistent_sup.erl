%%%----------------------------------------------------------------------
%%%
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-01-13
%%% @doc the mgee persistent data supervisor.
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(mgee_persistent_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start/0
        ]).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------

start() ->
	{ok, _} = supervisor:start_child(
	  mgee_sup, 
	  {mgee_persistent_sup,
	   {mgee_persistent_sup, start_link, []},
	   transient, infinity, supervisor, [mgee_persistent_sup]}),
	ok.

start_link() ->
    supervisor:start_link({local, mgee_persistent_sup}, ?MODULE, []).

init(Callback) ->
	?INFO_MSG("~p init: ~p",[?MODULE, Callback ]),
    {ok, {{one_for_one, 10, 10},
          [{mgee_persistent, {mgee_persistent, start_link, [Callback]},
            transient, brutal_kill, worker, [mgee_persistent]}]}}.


