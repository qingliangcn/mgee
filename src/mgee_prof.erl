%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-2-21
%%% @doc TODO: Add description to mgee_prof
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_prof).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).
-export([
		 setup_profiling/0
		 ]).

%%
%% API Functions
%%

eprof_start() ->
	case eprof:start() of
		{ok,EprofPid} ->
			PList = erlang:processes(),
			ZList = lists:filter( 
				fun(T) -> 
					case T of
						EprofPid -> 
							false;
						Pid when is_pid(Pid) ->
							true;
						_ ->
							false 	
					end
				end, PList ),
			eprof:start_profiling(ZList);
		{error,Reason} ->
			{error,Reason}
	end.

eprof_result() ->
	try
		fun() ->
			eprof:total_analyse()
		end
	catch
		X:Y -> {catch_error, X,Y} 
	end.

eprof_stop() ->
	try
		fun() ->
			eprof:stop_profiling(),
			eprof:stop()
		end
	catch
		X:Y -> {catch_error, X,Y} 
	end.


%%%%%%%%%%%%%%%%%%%%%

setup_profiling() ->
	case application:get_env(profiling_enabled) of
		{ok, true} ->
			fprof:trace(start);
		_ ->
			ok
	end.
			

fprof_start() ->
	case fprof:start() of
		{ok,FprofPid} ->
			PList = erlang:processes(),
			ZList = lists:filter( 
				fun(T) -> 
					case T of
						FprofPid -> 
							false;
						Pid when is_pid(Pid) ->
							true;
						_ ->
							false 	
					end
				end, PList ),
			case fprof:trace([start, {file, "mgee_fprof.trace"}, {procs, ZList}]) of
				ok ->
					ok;
				{error,Reason} ->
					{error,Reason};
				{'EXIT', ServerPid, Reason} ->
					{exit, ServerPid, Reason}
			end;
		{error,Reason} ->
			{error,Reason}
	end.

fprof_result() ->
	try
		fun() ->
			fprof:profile(),
			fprof:analyse({dest, "mgee_fprof.analysis.txt"})
		end
	catch
		X:Y -> {catch_error, X,Y} 
	end.

fprof_stop() ->
	try
		fun() ->
			fprof:trace(stop),
			fprof:stop()
		end
	catch
		X:Y -> {catch_error, X,Y} 
	end.


%%
%% Local Functions
%%

