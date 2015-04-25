%% @author asceth <machinist@asceth.com>
%% @since 14 Aug 2008 by asceth <machinist@asceth.com>
%% @doc memcached client library for erlang<br />
%% All functions accept an existing socket or a tuple
%% describing a host and port to connect to.
%% <br />
%% Variables:<br />
%% <ul>
%%   <li>Flags -> unique integer to associate with key/value pair</li>
%%   <li>Expire -> time in seconds to keep a key/value pair</li>
%% </ul>

-module(memcached).

%% External API
-export([set/3, set/5]).
-export([add/3, add/5]).
-export([replace/3, replace/5]).
-export([get/2]).
-export([delete/3, delete/2]).
-export([stats/1]).


%%====================================================================
%% Types
%%====================================================================
%% @type hostport() = {host, string(), port, integer()}. Tuple describing a host and port to connect to
%% @type socket() = {socket, port()}. Tuple describing an existing socket
%% @type memcached_connection() = hostport() | socket().
%% @type memcached_key() = list() | atom().
-type(hostport() :: {host, string(), port, integer()}).
-type(socket() :: {socket, port()}).
-type(memcached_connection() :: hostport() | socket()).
-type(memcached_key() :: list() | atom()).


%%====================================================================
%% External API
%%====================================================================
%% @doc Associate Bytes with Key.
%% @spec set(memcached_connection(), Key::memcached_key(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(set/3::(memcached_connection(), memcached_key(), any()) ->
         ok | {error, not_stored}).
set({host, Host, port, Port}, Key, Bytes) ->
  set({host, Host, port, Port}, Key, 0, 0, Bytes);
set({socket, Socket}, Key, Bytes) ->
  set({socket, Socket}, Key, 0, 0, Bytes).

%% @doc Associate Bytes with Key using Flags and Expire options.
%% @spec set(memcached_connection(), Key::memcached_key(), Flags::integer(), Expire::integer(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(set/5::(memcached_connection(), memcached_key(), integer(), integer(), any()) ->
         ok | {error, not_stored}).
set({host, Host, port, Port}, Key, Flags, Expire, Bytes) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_set(Socket, set, Key, Flags, Expire, Bytes),
  gen_tcp:close(Socket),
  Reply;
set({socket, Socket}, Key, Flags, Expire, Bytes) ->
  process_set(Socket, set, Key, Flags, Expire, Bytes).


%%====================================================================
%% @doc Associate Bytes with Key if Key isn't set already in memcached.
%% @spec add(memcached_connection(), Key::memcached_key(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(add/3::(memcached_connection(), memcached_key(), any()) ->
         ok | {error, not_stored}).
add({host, Host, port, Port}, Key, Bytes) ->
  add({host, Host, port, Port}, Key, 0, 0, Bytes);
add({socket, Socket}, Key, Bytes) ->
  add({socket, Socket}, Key, 0, 0, Bytes).

%% @doc Associate Bytes with Key using Flags and Expire options if Key isn't set already in memcached.
%% @spec add(memcached_connection(), Key::memcached_key(), Flags::integer(), Expire::integer(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(add/5::(memcached_connection(), memcached_key(), integer(), integer(), any()) ->
         ok | {error, not_stored}).
add({host, Host, port, Port}, Key, Flags, Expire, Bytes) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_set(Socket, add, Key, Flags, Expire, Bytes),
  gen_tcp:close(Socket),
  Reply;
add({socket, Socket}, Key, Flags, Expire, Bytes) ->
  process_set(Socket, add, Key, Flags, Expire, Bytes).


%%====================================================================
%% @doc Associate Bytes with Key if Key is set already in memcached.
%% @spec replace(memcached_connection(), Key::memcached_key(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(replace/3::(memcached_connection(), memcached_key(), any()) ->
         ok | {error, not_stored}).
replace({host, Host, port, Port}, Key, Bytes) ->
  replace({host, Host, port, Port}, Key, 0, 0, Bytes);
replace({socket, Socket}, Key, Bytes) ->
  replace({socket, Socket}, Key, 0, 0, Bytes).

%% @doc Associate Bytes with Key using Flags and Expire options if Key is set already in memcached.
%% @spec replace(memcached_connection(), Key::memcached_key(), Flags::integer(), Expire::integer(), Bytes::any()) ->
%%         ok | {error, not_stored}
-spec(replace/5::(memcached_connection(), memcached_key(), integer(), integer(), any()) ->
         ok | {error, not_stored}).
replace({host, Host, port, Port}, Key, Flags, Expire, Bytes) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_set(Socket, replace, Key, Flags, Expire, Bytes),
  gen_tcp:close(Socket),
  Reply;
replace({socket, Socket}, Key, Flags, Expire, Bytes) ->
  process_set(Socket, replace, Key, Flags, Expire, Bytes).


%%====================================================================
%% @doc Return value associated with Key.  Will automatically convert
%%       back to erlang terms.  Key can be a single key or a list of
%%       keys.
%% @spec get(memcached_connection(), Key::memcached_key() | [Key::memcached_key()]) ->
%%         [any()]
-spec(get/2::(memcached_connection(), memcached_key() | [memcached_key()]) ->
         [any()]).
get({host, Host, port, Port}, [Head|Tail]) when is_list(Head) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_get(Socket, [Head] ++ Tail),
  gen_tcp:close(Socket),
  Reply;
get({host, Host, port, Port}, Key) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_get(Socket, [Key]),
  gen_tcp:close(Socket),
  Reply;

get({socket, Socket}, [Head|Tail]) when is_list(Head) ->
  process_get(Socket, [Head] ++ Tail);
get({socket, Socket}, Key) ->
  process_get(Socket, [Key]).


%%====================================================================
%% @doc Delete a key from memcached
%% @spec delete(memcached_connection(), Key::memcached_key()) ->
%%         ok | {error, not_found}
-spec(delete/2::(memcached_connection(), memcached_key()) ->
         ok | {error, not_found}).
delete({host, Host, port, Port}, Key) ->
  delete({host, Host, port, Port}, Key, 0);
delete({socket, Socket}, Key) ->
  delete({socket, Socket}, Key, 0).

%% @doc Delete a key from memcached after Time seconds
%% @spec delete(memcached_connection(), Key::memcached_key(), Time::integer()) ->
%%         ok | {error, not_found}
-spec(delete/3::(memcached_connection(), memcached_key(), integer()) ->
         ok | {error, not_found}).
delete({host, Host, port, Port}, Key, Time) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_delete(Socket, Key, Time),
  gen_tcp:close(Socket),
  Reply;
delete({socket, Socket}, Key, Time) ->
  process_delete(Socket, Key, Time).


%%====================================================================
%% @doc Delete a key from memcached
%% @spec stats(memcached_connection()) ->
%%         [string()]
-spec(stats/1::(memcached_connection()) ->
         [string()]).
stats({host, Host, port, Port}) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  Reply = process_stats(Socket),
  gen_tcp:close(Socket),
  Reply;
stats({socket, Socket}) ->
  process_stats(Socket).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

to_list(Key) when is_atom(Key) ->
  atom_to_list(Key);
to_list(Key) when is_binary(Key) ->
  binary_to_list(Key);
to_list(Key) when is_list(Key) ->
  Key.

fetch_more(Socket, Len, More) ->
  %% Read what we need to grab the data.
  {ok, <<Data/binary>>} = gen_tcp:recv(Socket, Len - size(More)),
  Combined = <<More/binary, Data/binary>>,
  if
    size(Combined) < Len ->
      {Bytes, Rest} = fetch_more(Socket, Len, Combined),
      {Bytes, Rest};
    true ->
      <<Bytes:Len/binary>> = Combined,
      %% Read anything left.
      {ok, <<Rest/binary>>} = gen_tcp:recv(Socket, 0),
      {Bytes, Rest}
  end.


%% Parse the get response
parse_responses(_Socket, <<"END\r\n", _Rest/binary>>, Acc) ->
  Acc;
parse_responses(Socket, <<"\r\n", Data/binary>>, Acc) ->
  parse_responses(Socket, Data, Acc);
parse_responses(Socket, <<"VALUE ", Data/binary>>, Acc) ->
  {ok, [_, _, Len], More} = io_lib:fread("~s ~u ~u\r\n", binary_to_list(Data)),
  if
    %% 5 is size(<<"\r\nEND\r\n">>)
    length(More) < (Len + 7) ->
      %% If we didnt' read all the data, fetch the rest
      {Bytes, Rest} = fetch_more(Socket, Len, list_to_binary(More)),
      parse_responses(Socket, Rest, Acc ++ [b2t(Bytes)]);
    true ->
      <<Bytes:Len/binary, Rest/binary>> = list_to_binary(More),
      parse_responses(Socket, Rest, Acc ++ [b2t(Bytes)])
  end.

b2t(Binary) ->
    try binary_to_term(Binary)
    catch
	_:_ -> Binary
    end.


%% Send get and handle the response
process_get(Socket, Keys) ->
  KeyList = [to_list(X) || X <- Keys],
  ok = gen_tcp:send(Socket, list_to_binary(["get ", string:join(KeyList, " "), "\r\n"])),
  {ok, <<Data/binary>>} = gen_tcp:recv(Socket, 0),
  parse_responses(Socket, Data, []).

%% Send set and handle the response
process_set(Socket, Operation, Key, Flags, Expire, Data) when not(is_binary(Data)) ->
    process_set(Socket, Operation, Key, Flags, Expire, term_to_binary(Data));
process_set(Socket, Operation, Key, Flags, Expire, Bytes) ->
  Op = atom_to_list(Operation),
  K = to_list(Key),
  Len = size(Bytes),
  L = list_to_binary( io_lib:format("~s ~s ~p ~p ~p",
                                    [Op, K, Flags, Expire, Len])),
  Line = <<L/binary, "\r\n">>,
  ok = gen_tcp:send(Socket, Line),
  ok = gen_tcp:send(Socket, <<Bytes/binary, "\r\n">>),
  {ok, Response} = gen_tcp:recv(Socket, 0),
  case Response of
    <<"STORED\r\n">> ->
      ok;
    <<"NOT_STORED\r\n">> ->
      {error, not_stored}
  end.

%% Send delete and handle the response
process_delete(Socket, Key, Time) ->
  Line = list_to_binary(io_lib:format("delete ~s ~p\r\n",
                                      [to_list(Key), Time])),
  ok = gen_tcp:send(Socket, Line),
  {ok, Response} = gen_tcp:recv(Socket, 0),
  case Response of
    <<"DELETED\r\n">> ->
      ok;
    <<"NOT_FOUND\r\n">> ->
      {error, not_found}
  end.

%% Send stats and handle the response
process_stats(Socket) ->
  Line = <<"stats\r\n">>,
  ok = gen_tcp:send(Socket, Line),
  {ok, Response} = gen_tcp:recv(Socket, 0),
  string:tokens(binary_to_list(Response), "\r\n").

