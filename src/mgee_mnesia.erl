%%%----------------------------------------------------------------------
%%% File    : mgee_mnesia.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_mnesia).

%%
%% Include files
%%
-include("mgee.hrl").
%%
%% Exported Functions
%%
-export([init/0]).

%%
%% API Functions
%%

init() ->
	?INFO_MSG("~p init: ~p",[?MODULE, [] ]),
	mnesia:create_table(mgee_tcp_listener, 
						[{ram_copies, [node()]}, 
						 {attributes, record_info(fields, listener)}]).

%%
%% Local Functions
%%

