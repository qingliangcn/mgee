%%%-------------------------------------------------------------------
%%% @author odinxu
%%% @date 2010-01-11
%%% @desc the control module
%%%-------------------------------------------------------------------

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

