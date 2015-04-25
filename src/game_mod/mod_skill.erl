%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2010-2-4
%%% -------------------------------------------------------------------
-module(mod_skill).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgee.hrl").
-include("game_pb.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([
		 start/1,
		 get_skill_info/1,
		 start_link/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

get_skill_info(SkillId) ->
	?TEST_MSG("search skill ~p", [SkillId]),
	case ets:lookup(ets_skill_list, SkillId) of
		[{SkillId, SkillInfo}] ->
			{ok, SkillInfo};
		[] ->
			{error, not_found}
	end.


%% ====================================================================
%% Server functions
%% ====================================================================
start(SkillFilePath) ->
	supervisor:start_child(mgee_sup, {mod_skill,
                {mod_skill, start_link,[SkillFilePath]},
                permanent, brutal_kill, worker, [mgee_sup]}).

start_link(SkillFilePath) ->
	gen_server:start_link({local, mod_skill}, ?MODULE, [SkillFilePath], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([SkillFilePath]) ->
	?INFO_MSG("~p init: ~p",[?MODULE, [SkillFilePath] ]),
	ets:new(ets_skill_list, [set, protected, named_table]),
	load_skill(SkillFilePath),
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
handle_call(Request, From, State) ->
	?TEST_MSG("unexpected call msg ~p from ~p", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?TEST_MSG("unexpected cast msg ~p", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?TEST_MSG("unexpected info msg ~p", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?TEST_MSG("mod_skill terminated :~p, state:~p", [Reason, State]),
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

load_skill(FilePath) ->
	{ok, [SkillList]} = file:consult(FilePath),
	lists:foreach(
		fun(S) -> 
			case erlang:is_record(S, p_skill) of
				true ->
					ets:insert(ets_skill_list, {S#p_skill.skill_id, S});
				false ->
					?TEST_MSG("ignore wrong skill info ~p", [S])
			end
		end, 
		SkillList).