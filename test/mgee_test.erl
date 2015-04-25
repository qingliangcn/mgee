%% Author: odinxu
%% Created: 2010-1-3
%% Description: test socket connect
%%
%% before run this program, you must editor file: "mgee_test.cfg"
%% { mgee_test, 
%%  [
%%    { host, "192.168.1.51"},
%%    { port, 8888 },
%%    { process_count , 10 },
%%    { timeout, 2000 }
%%  ]
%% }.
%%
%% 
%%
-module(mgee_test).


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
	start( get_configure( process_count ) ).

start(N) when N>0->
	%%spawn(?MODULE, start_test_proc,[N]),
	spawn(fun() -> start_test_proc(N) end ),
	sleep(100),
	start(N-1);

start(_) ->
	ok.

%%
%% Local Functions
%%


get_configure( T ) ->
	{ ok, [ {mgee_test, L} ] } = file:consult("./mgee_test.cfg"),
	[{T, V}] = lists:filter(
		fun(F) ->  
			case F of
				{ T, _Val} -> true;
				_ -> false
			end
		end	, L
		), 
	V.

start_test_proc(N) ->
	case gen_tcp:connect(get_configure( host ), 
						 get_configure( port ),
						[binary, {packet, 2}, {active, false}], 
						get_configure( timeout )   ) of
		{ok, LSock} -> 
			%%spawn(?MODULE, test_send_msg, [N, LSock]),
			spawn(fun() -> test_recv_msg(N, LSock) end ),
			test_loop(N, LSock);
		{error, Reason} -> 
			io:format("~p start_test_proc ~p fail, reason: ~p~n", [self(), N, Reason ]),
			exit( {error, N, Reason})
	end.

%%=====================================================
%%开一个进程，循环接受服务器发过来的消息，并显示
test_recv_msg(N, LSock) ->
  _Req = do_recv(LSock),
  %%io:format("~p ~p ~p receive msg: ~p~n", [self(), LSock, N, Req]),
  test_recv_msg(N, LSock).

%%自动间隔一段时间，就向服务器发送一条消息
test_loop(N, LSock) ->
	test_loop(N, LSock, 0).

%%=====================================================
test_loop(N, LSock, M) ->
	T=random(1000,2000),
	io:format("~p ~p ~p sleep ~p ms~n", [self(), LSock, N, T] ),
	sleep(T),
	do_send(LSock, <<"msg", N, M>>),
	test_loop(N, LSock, M+1).


%%=====================================================
%% send a line of text to the socket
do_send(LSock,Msg) ->
  case gen_tcp:send(LSock, Msg) of
    ok -> ok;
    {error, Reason} -> 
		io:format("~p ~p send msg fail. ~p~n", [self(), LSock, Msg] ),
		exit(Reason)
  end.

%% receive data from the socket
do_recv(LSock) ->
  case gen_tcp:recv(LSock, 0) of
    {ok, Bin} -> binary_to_list(Bin);
    {error, Reason} -> 
		io:format("~p ~p receive socket fail.~n", [self(), LSock] ),
		exit(Reason)
  end.

%%=====================================================
%% 暂停 毫秒
sleep(Msec) ->
	receive
		after Msec ->
			true
	end.

%% 随机数
random(Min,Max)->
	Min2 = Min-1,
	random:uniform(Max-Min2)+Min2.


