%%%----------------------------------------------------------------------
%%% File    : mgee_router.erl
%%% Author  : Qingliang
%%% Created : 2010-1-5
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_router).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
		 router/1, 
		 reload_router_map/1, 
		 start/0, 
		 start_link/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	{ok, _} = supervisor:start_child(mgee_sup, {mgee_router,
												{mgee_router, start_link, []},
												transient, brutal_kill, worker, [mgee_router]
												}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], ?GEN_SERVER_OPTIONS).


%% ====================================================================
%% Server functions
%% ====================================================================

router({ClientSock, Module, Method, <<>> , AccountName, RoleId, RoleName}) ->
    ?DEBUG("router ~p ~p ~p", [Module, Method, <<>>]),
	case ets:lookup(mgee_router_map, Module) of
				[{Module, ModuleReal}] -> 
					Args = {ClientSock, Module, Method, none, AccountName, RoleId, RoleName},
					case (catch apply(ModuleReal, handle, [Args])) of
						{'EXIT', Info} -> 
							?TEST_MSG(" ~p", [Info]);
						Atom when is_atom(Atom) ->
							none;
						DataRtn ->
							mgee_packet:packet_encode_send2(ClientSock, Module, Method, DataRtn)
					end;
				_ -> 
					?TEST_MSG("undefined module ~p", [Module]), 
					ok
	end;
	
router({ClientSock, Module, Method, Data, AccountName, RoleId, RoleName}) ->
    ?DEBUG("router ~p ~p ~p", [Module, Method, Data]),
	case (catch mgee_packet:decode(Module, Method, Data)) of
		{'EXIT', Info} -> 
			?ERROR_MSG("error when handle Module:~p, Method:~p, Info:~p", 
				   [Module, Method, Info]),
			exit(mgee_misc:account_process_name(AccountName), hack_attemp);
		DataRecord ->
			?TEST_MSG("~p", [DataRecord]),
			case ets:lookup(mgee_router_map, Module) of
				[{Module, ModuleReal}] -> 
					Args = {ClientSock, Module, Method, DataRecord, AccountName, RoleId, RoleName},
					case (catch apply(ModuleReal, handle, [Args])) of
						{'EXIT', Info} -> 
							?TEST_MSG(" ~p", [Info]);
						Atom when is_atom(Atom) ->
							none;
						DataRtn ->
							mgee_packet:packet_encode_send2(ClientSock, Module, Method, DataRtn)
					end;
				_ -> 
					?TEST_MSG("undefined module ~p", [Module]), 
					ok
			end
	end.

%% use this method to update the moudle method map data.
reload_router_map(Filename) ->
	{ok, _Map} = file:consult(Filename),
	ok.

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
	List = [
			{<<"vw">>, 		mgee_virtual_world_router},
			{<<"move">>, 	mgee_move},
      		{<<"chat">>, 	mgee_chat},
			{<<"role">>, 	mgee_account_server},
			{<<"login">>, 	mgee_account_server},
			{<<"team">>, 	mod_team_server		},
			{<<"family">>, 	mod_family_server	},
			{<<"fight">>,	mod_fight},
			{<<"equip">>, 	mod_equip}
			],
	ets:new(mgee_router_map, [set, protected, named_table]),
	lists:foreach( 
		fun(M) ->
			ets:insert(mgee_router_map, M)
		end,
		List),
    {ok, none}.

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

