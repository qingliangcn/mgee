%%%----------------------------------------------------------------------
%%% File    : mgee_loglevel.erl
%%% Author  : Qingliang
%%% Purpose : Manage Erlang logging.
%%% Created : 2010-01-01
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% File    : ejabberd_loglevel.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Loglevel switcher.
%%%           Be careful: you should not have any ejabberd_logger module
%%%           as ejabberd_loglevel switcher is compiling and loading
%%%           dynamically a "virtual" ejabberd_logger module (Described
%%%           in a string at the end of this module).
%%% Created : 29 Nov 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mgee_loglevel).

-export([set/1, get/0]).

-include("mgee.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-define(LOG_LEVELS,[ {0, no_log, "No log"}
                    ,{1, critical, "Critical"}
                    ,{2, error, "Error"}
                    ,{3, warning, "Warning"}
                    ,{4, info, "Info"}
                    ,{5, debug, "Debug"}
					,{6, test, "Test"}
                    ]).

get() ->
    Level = ejabberd_logger:get(),
    case lists:keysearch(Level, 1, ?LOG_LEVELS) of
        {value, Result} -> Result;
        _ -> erlang:error({no_such_loglevel, Level})
    end.


set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));
set(Loglevel) when is_integer(Loglevel) ->
    try
        {Mod,Code} = dynamic_compile:from_string(mgee_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> ?CRITICAL_MSG("Error compiling logger (~p): ~p~n", [Type, Error])
    end;
set(_) ->
    exit("Loglevel must be an integer").

level_to_integer(Level) ->
    case lists:keysearch(Level, 2, ?LOG_LEVELS) of
        {value, {Int, Level, _Desc}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the ejabberd logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
mgee_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(mgee_logger).

    -export([test_msg/4,
			 debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             get/0]).

   get() -> "++ L ++".

    %% Helper functions
    test_msg(Module, Line, Format, Args) when " ++ L ++ " >= 6 ->
            notify(info_msg,
                   \"T(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    test_msg(_,_,_,_) -> ok.

    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
		case Args of
			%% start with : ** Node php
			[42,42,32,78,111,100,101,32,112,104,112|_] ->
				ok;
			_ ->
            	notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args)
		end;
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
