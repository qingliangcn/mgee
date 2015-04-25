%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-3-2
%%% @doc TODO: Add description to fastcgi
%%% @end
%%%----------------------------------------------------------------------

-module(fastcgi).

-define(FCGI_LOCALHOST, "127.0.0.1").
-define(FCGI_PORT, 10080).
-define(FCGI_DOMAIN, "localhost").
-define(FCGI_DOCUMENT_ROOT, "/data/web/fcgi" ).
-define(FCGI_TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
-define(FCGI_TCP_TIMEOUT, 1000).

-define(HTTP_CONTENT_TYPE_URL_PARAM, "application/x-www-form-urlencoded" ).
-define(HTTP_CONTENT_TYPE_AMF_DATA, "application/x-amf" ).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		get/2,
		post/2,
		post_amf/2,
		
		test_g/0,
		test_p/0,
		get/5,
		post/6
		]).

%%
%% API Functions
%%

% get("/i.php", "pa=1&pb=22")
get(Url, Param) ->
	get(?FCGI_LOCALHOST, ?FCGI_PORT, ?FCGI_DOMAIN, Url, Param).

% post("/i.php", "pa=1&pb=22")
post(Url, PostData) ->
	post(?FCGI_LOCALHOST, ?FCGI_PORT, ?FCGI_DOMAIN, Url, PostData, ?HTTP_CONTENT_TYPE_URL_PARAM).

% post("/i.php", BinaryAmfData)
post_amf(Url, PostData) when is_list(PostData)->
	post(?FCGI_LOCALHOST, ?FCGI_PORT, ?FCGI_DOMAIN, Url, PostData, ?HTTP_CONTENT_TYPE_AMF_DATA);	
post_amf(Url, PostData) when is_binary(PostData)->
	post(?FCGI_LOCALHOST, ?FCGI_PORT, ?FCGI_DOMAIN, Url, binary_to_list(PostData), ?HTTP_CONTENT_TYPE_AMF_DATA).	


get(HostIp, Port, ServerName, Url, ParamData) ->
	Data = make_string_get(HostIp, ServerName, Url, ParamData),
	fcgi_send_data_and_get_result(HostIp, Port, Data).

post(HostIp, Port, ServerName, Url, PostData, ContentType) ->
	Data = make_string_post(HostIp, ServerName, Url, PostData, ContentType),
	fcgi_send_data_and_get_result(HostIp, Port, Data).

%%
%% Local Functions
%%

test_g() ->
	test_get(?FCGI_LOCALHOST, ?FCGI_PORT, "73.217.ming.cn", "/i.php", "parama=aaaaaaa&paramb=bbbbbbb&paramc=cccccccccc").

test_p() ->
	test_post(?FCGI_LOCALHOST, ?FCGI_PORT, "73.217.ming.cn", "/i.php", "type=post&parama=aaaaaaa&paramb=bbbbbbb&paramc=cccccccccc").

test_get(HostIp, Port, ServerName, Url, ParamData) ->
	Data = make_string_get(HostIp, ServerName, Url, ParamData),
	io:format("input data: ~p~n", [Data]),
	file:write_file("get.out", Data),
	case fcgi_send_data_and_get_result(HostIp, Port, Data) of
		{error, Reason} ->
			io:format("fcgi_get_call error:~p~n", [Reason] );
		Result ->
			io:format("fcgi_get_call ok, result length:~p~n", [length(Result)]),
			file:write_file("get.result", Result)
	end,
	ok.

test_post(HostIp, Port, ServerName, Url, PostData) ->
	Data = make_string_post(HostIp, ServerName, Url, PostData, ?HTTP_CONTENT_TYPE_URL_PARAM),
	io:format("post input data: ~p~n", [Data]),
	file:write_file("post.out", Data),
	case fcgi_send_data_and_get_result(HostIp, Port, Data) of
		{error, Reason} ->
			io:format("fcgi_post_call error:~p~n", [Reason] );
		Result ->
			io:format("fcgi_post_call ok, result length:~p~n", [length(Result)]),
			file:write_file("post.result", Result)
	end,
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fcgi_send_data_and_get_result(HostIp, Port, Data) ->
	case gen_tcp:connect(HostIp, Port, ?FCGI_TCP_OPTIONS, ?FCGI_TCP_TIMEOUT  ) of
		{ok, LSock} ->
			Bin = list_to_binary(Data),
			case gen_tcp:send(LSock, Bin) of
				ok ->
					%io:format("~p ~p send msg ok. msg length:~p~n", [self(), LSock, erlang:byte_size(Bin)] ), 
					recv_fcgi_result(LSock);

				{_, Reason} -> 
					%io:format("~p ~p send msg fail.~p~n", [self(), LSock, Reason] ),
					{error, Reason} 
  			end;
		{error, Reason} -> 
			%io:format("~p connect ~p:~p fail. Reason: ~p~n", [self(), HostIp, Port, Reason] ),
			{error, Reason} 
	end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recv_fcgi_result(LSock) ->
	recv_fcgi_result(LSock, 1, []).

recv_fcgi_result(LSock, N, Result) ->
  case gen_tcp:recv(LSock, 8, ?FCGI_TCP_TIMEOUT) of
	% type is 6, means stdout data
    {ok, [1, 6, _, _, ContentLenB1, ContentLenB0, PaddingLen, _] } ->
		RecvLen = ContentLenB1 * 256 + ContentLenB0,
		if RecvLen > 0 ->
		  case gen_tcp:recv(LSock, ContentLenB1 * 256 + ContentLenB0, ?FCGI_TCP_TIMEOUT) of
			{ok, Data} ->
				% skip the padding data
				if PaddingLen > 0 ->
					gen_tcp:recv(LSock, PaddingLen, ?FCGI_TCP_TIMEOUT);
				true ->
					ok
				end,
				case N of
					1 ->
						HeaderPos  = string:str(Data, [16#0d, 16#0a, 16#0d, 16#0a]),
						%DataHeader = string:substr(Data, 1, HeaderPos - 1),
						DataBody   = string:substr(Data, HeaderPos + 4),
						recv_fcgi_result(LSock, N+1, DataBody );
					_ ->	
						recv_fcgi_result(LSock, N+1, lists:concat([Result, Data]) )
				end;				
			{error, closed} ->
				Result;
    		{_, Reason} ->
				{error, Reason} 
		  end;
		true ->
		  Result
		end;
	% type is 3, means data end
    {ok, [1, 3, _, _, ContentLenB1, ContentLenB0, PaddingLen, _] } ->
		%ingore some data
		RecvLen = ContentLenB1 * 256 + ContentLenB0 + PaddingLen,
		if RecvLen > 0 ->
				gen_tcp:recv(LSock, ContentLenB1 * 256 + ContentLenB0 + PaddingLen, ?FCGI_TCP_TIMEOUT);
			true ->
				ok
		end,
		gen_tcp:close(LSock),
		Result;
	{ok, Other} ->
		{error, Other};
	{error, closed} ->
		Result;
    {_, Reason} ->
		{error, Reason} 
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param example:
% HostIp = "127.0.0.1"
% ServerName = "www.abc.com"
% Url = "/i.php"
% ParamData = "parama=aaaaaaa&paramb=bbbbbbb&paramc=cccccccc"
make_string_get(HostIp, ServerName, Url, ParamData) ->
	ServerNameLen = length(ServerName),
	UrlLen   = length(Url),
	ParamLen = length(ParamData),
	DocuRootLen = length(?FCGI_DOCUMENT_ROOT),

%	REMOTE_ADDR = "127.0.0.1",
%	REMOTE_PORT = "60000",

	ParamList = 
	[17, 7, 					"GATEWAY_INTERFACE", 	"CGI/1.1",
	 15, 5, 					"SERVER_SOFTWARE", 		"mgeee",
	 12, ParamLen, 				"QUERY_STRING", 		ParamData,
	 14, 3, 					"REQUEST_METHOD", 		"GET",
	 12, 0, 					"CONTENT_TYPE", 
	 14, 0, 					"CONTENT_LENGTH",	
	 15, DocuRootLen + UrlLen, 	"SCRIPT_FILENAME", 		?FCGI_DOCUMENT_ROOT, Url,
	 11, UrlLen, 				"SCRIPT_NAME", 			Url,
	 11, UrlLen + ParamLen + 1, "REQUEST_URI", 			Url, "?", ParamData,
	 12, UrlLen, 				"DOCUMENT_URI", 		Url,
	 13, DocuRootLen, 			"DOCUMENT_ROOT", 		?FCGI_DOCUMENT_ROOT,
	 15, 8,						"SERVER_PROTOCOL", 		"HTTP/1.1",
%	 11, length(REMOTE_ADDR),	"REMOTE_ADDR",			REMOTE_ADDR,
%	 11, length(REMOTE_PORT),	"REMOTE_PORT",			REMOTE_PORT,
	 11, length(HostIp),		"SERVER_ADDR", 			HostIp,
%	 11, 2,						"SERVER_PORT", 			"80",
	 11, ServerNameLen,			"SERVER_NAME", 			ServerName, 
	
	 % the first HTTP_ data, must add two 0 bytes.
%	 11, 128, 0, 0,	164,		"HTTP_ACCEPT",				"image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*",
	 11, 128, 0, 0,	3,			"HTTP_ACCEPT",				"*/*",
%	 20, 5,						"HTTP_ACCEPT_LANGUAGE",		"zh-cn",
%	 20, 13,					"HTTP_ACCEPT_ENCODING",		"gzip, deflate",
%	 15, 61,					"HTTP_USER_AGENT",			"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; CIBA)",
	 9,  ServerNameLen,			"HTTP_HOST", 				ServerName, 
	 15, 10,					"HTTP_CONNECTION",			"Keep-Alive"	],

	ParamListLen = erlang:byte_size(list_to_binary(ParamList)),
	<< PLL1:8, PLL0:8 >> = << ParamListLen:16 >>,	
	TmpValue = ParamListLen + 8,
	<< _:12, PP0:4 >> = << TmpValue:16 >>,
	PaddingLen = 16 - PP0,
	%io:format("padding length:~p~n",[PaddingLen]),

	FCGI_Header =
	[1, 1, 0, 1, 0, 8, 0, 0,
	 0, 1, 0, 0, 0, 0, 0, 0,
	 1, 4, 0, 1, PLL1, PLL0, PaddingLen, 0 ],

	PaddingData = lists:duplicate(PaddingLen, 0),
	FCGI_Header2 = 
	[1, 4, 0, 1, 0, 0, 0, 0,
	 1, 5, 0, 1, 0, 0, 0, 0 ],

	lists:concat([ FCGI_Header, ParamList
					,PaddingData, FCGI_Header2 ] 
				).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param example:
% HostIp = "127.0.0.1"
% ServerName = "www.abc.com"
% Url = "/i.php"
% PostData = "parama=aaaaaaa&paramb=bbbbbbb&paramc=cccccccc"
make_string_post(HostIp, ServerName, Url, PostData, ContentType) ->
	ServerNameLen = length(ServerName),
	UrlLen   = length(Url),
	PostDataLen = length(PostData),
	DocuRootLen = length(?FCGI_DOCUMENT_ROOT),
	ContentTypeLen = length(ContentType),
	
	CONTENT_LENGTH = integer_to_list(PostDataLen),
	CONTENT_LENGTH_Len = length(CONTENT_LENGTH),
	
%	REMOTE_ADDR = "127.0.0.1",
%	REMOTE_PORT = "60000",

	ParamList = 
	[17, 7, 					"GATEWAY_INTERFACE", 	"CGI/1.1",
	 15, 5, 					"SERVER_SOFTWARE", 		"mgeee",
	 12, 0,		 				"QUERY_STRING", 
	 14, 4, 					"REQUEST_METHOD", 		"POST",
	 12, ContentTypeLen,		"CONTENT_TYPE", 		ContentType, 
	 14, CONTENT_LENGTH_Len,	"CONTENT_LENGTH",		CONTENT_LENGTH,
	 15, DocuRootLen + UrlLen, 	"SCRIPT_FILENAME", 		?FCGI_DOCUMENT_ROOT, Url,
	 11, UrlLen, 				"SCRIPT_NAME", 			Url,
	 11, UrlLen, 				"REQUEST_URI",	 		Url, 
	 12, UrlLen, 				"DOCUMENT_URI", 		Url,
	 13, DocuRootLen, 			"DOCUMENT_ROOT", 		?FCGI_DOCUMENT_ROOT,
	 15, 8,						"SERVER_PROTOCOL", 		"HTTP/1.1",
%	 11, length(REMOTE_ADDR),	"REMOTE_ADDR",			REMOTE_ADDR,
%	 11, length(REMOTE_PORT),	"REMOTE_PORT",			REMOTE_PORT,
	 11, length(HostIp),		"SERVER_ADDR", 			HostIp,
%	 11, 2,						"SERVER_PORT", 			"80",
	 11, ServerNameLen,			"SERVER_NAME", 			ServerName, 
	
	 % the first HTTP_ data, must add two 0 bytes.
%	 11, 128, 0, 0,	164,		"HTTP_ACCEPT",				"image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*",
	 11, 128, 0, 0,	3,			"HTTP_ACCEPT",				"*/*",
%	 20, 5,						"HTTP_ACCEPT_LANGUAGE",		"zh-cn",
%	 20, 13,					"HTTP_ACCEPT_ENCODING",		"gzip, deflate",
%	 15, 61,					"HTTP_USER_AGENT",			"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; CIBA)",
	 12, 5,						"HTTP_REFERER",				"mgeee",
	 9,  ServerNameLen,			"HTTP_HOST", 				ServerName, 
	 17, ContentTypeLen,		"HTTP_CONTENT_TYPE", 		ContentType, 
	 19, CONTENT_LENGTH_Len,	"HTTP_CONTENT_LENGTH",		CONTENT_LENGTH,	 
	 18, 8,						"HTTP_CACHE_CONTROL",		"no-cache",
	 15, 10,					"HTTP_CONNECTION",			"Keep-Alive"	],

	ParamListLen = erlang:byte_size(list_to_binary(ParamList)),
	<< PLL1:8, PLL0:8 >> = << ParamListLen:16 >>,	
	<< _:12, PP0:4 >> = << ParamListLen:16 >>,
	PaddingLen = 16 - PP0,
	%io:format("param list padding length:~p~n",[PaddingLen]),

	FCGI_Header =
	[1, 1, 0, 1, 0, 8, 0, 0,
	 0, 1, 0, 0, 0, 0, 0, 0,
	 1, 4, 0, 1, PLL1, PLL0, PaddingLen, 0 ],

	<< PDL1:8, PDL0:8 >> = << PostDataLen:16 >>,	
	<< _:12, PD0:4 >> = << PostDataLen:16 >>,
	PostDataPaddingLen = 16 - PD0,
	%io:format("post data padding length:~p~n",[PostDataPaddingLen]),
	
	FCGI_Header2 = 
	[1, 4, 0, 1, 0, 0, 0, 0,
	 1, 5, 0, 1, PDL1, PDL0, PostDataPaddingLen, 0 ],
	
	FCGI_Header3 =	[1, 5, 0, 1, 0, 0, 0, 0 ],	

	lists:concat([  FCGI_Header, ParamList
					, lists:duplicate(PaddingLen, 0), FCGI_Header2
				 	, PostData
				 	, lists:duplicate(PostDataPaddingLen, 0), FCGI_Header3				   
				   ]
				).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%