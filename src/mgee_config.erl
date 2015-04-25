%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-18
%%% @doc when start application, read all configure, and save to ets(memory),
%%% 	use mgee_config:read() function to get a config value.
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_config).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("mgee_vo.hrl").
-include("global_lang.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
		 read/2
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(config_state, {test}).

%% ====================================================================
%% External functions
%% ====================================================================


read(_Module, _Key) ->
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

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
	ets:new(mgee_config_ets, [set, protected, named_table]),
	
	init_all(),
    {ok, #config_state{test=1}}.

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
get_configure_file_path() ->
	{ok, _ConfigPath} = application:get_env(config_path).

init_map(ConfigPath)->
	Ets 	= ets:new(config_ets_map, [set,protected,named_table,{keypos,1}]),
	load(Ets,	ConfigPath ++ "mcc/map/map.yrl"),
	?DEBUG("  Init Map ok.. ~n", []),
	ok.

init_npc(ConfigPath) ->
	Ets 	= ets:new(config_ets_npc, [set,protected,named_table,{keypos,1}]),
	load(Ets,	ConfigPath ++ "mcc/npc/npc.yrl"),
	?DEBUG("  Init Npc ok.. ~n", []),
	ok.

init_all() ->
	ConfigPath = get_configure_file_path(),
	init_map(ConfigPath),
	init_npc(ConfigPath),
	ok.




%% 内部函数数
load(Ets,FileName)->
	load(Ets,FileName,fun(_Data)->ok end).
load(Ets,FileName,Fun) ->
	load(Ets,FileName,Fun,fun(Data)->Data end).
load(Ets,FileName,none,DataFun) ->
	load(Ets,FileName,fun(_Data)->ok end,DataFun);
load(Ets,FileName,Fun,DataFun)->
	?TEST_MSG("load yrl file: ~p",[FileName]),
	case file:consult(FileName) of
        {ok, [Datas|_]} ->
            lists:foreach(fun(Data)->
								  Fun(Data),
								  ets:insert(Ets,DataFun(Data))
						  end, Datas);
        {error, Why2} ->
            exit({FileName, Why2})
    end.





