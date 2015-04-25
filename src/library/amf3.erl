%%%-------------------------------------------------------------------
%%% File:      amf3.erl
%%% @author    Brian Buchanan <bwb@holo.org>
%%% @copyright 2008 Brian Buchanan
%%% @doc    
%%% Codec for Adobe Flash AMF3 encoding.
%%% @end  
%%%-------------------------------------------------------------------
-module(amf3).
%% API
-export([encode/1, decode/1, decode/2]).

%% AMF3 wire types
-define(AMF3_UNDEF,     16#00). 
-define(AMF3_NULL,      16#01). 
-define(AMF3_FALSE,     16#02).
-define(AMF3_TRUE,      16#03).
-define(AMF3_INTEGER,   16#04).
-define(AMF3_NUMBER,    16#05).
-define(AMF3_STRING,    16#06).
-define(AMF3_XMLDOC,    16#07).  %% legacy flash.xml.XMLDocument
-define(AMF3_DATE,      16#08).
-define(AMF3_ARRAY,     16#09).
-define(AMF3_OBJECT,    16#0a).
-define(AMF3_XML,       16#0b).  %% E4X XML document
-define(AMF3_BYTEARRAY, 16#0c).

%% AMF3 variable-length integers are 29-bit
-define(AMF3_INT_MIN, -16#10000000).
-define(AMF3_INT_MAX,  16#0fffffff).

%% AMF3 object header flags
-define(AMF3_CLASS_STATIC,  2#0011).
-define(AMF3_CLASS_DYNAMIC, 2#1011).
-define(AMF3_CLASS_EXTERN,  2#0111).
-define(AMF3_CLASS_DEXTERN, 2#1111).

-type(amf3_integer() :: -16#10000000..16#0fffffff).
-type(string_like() :: binary() | atom() | iolist()).

-type(decode_opt() :: {keys, atom | existing_atom | binary | list | existing_atom_or_list} |
                      {byte_array, binary | wrapped}).

-record(amf3_class, {name::string_like(), keys::[string_like()], flags::0..2#1111}).
-record(state, {output_buffer=[],
                decode_keys_as=existing_atom, decode_bytearray_as=binary,
                next_complex_ref=0, complex_refs,
                next_string_ref=0, string_refs,
                next_class_ref=0, class_refs}).

%%====================================================================
%% API
%%====================================================================
-spec(encode/1 :: (any()) -> binary()).
encode(Value) ->
    State1 = output_value(Value,
                          #state{complex_refs=dict:new(),
                                 string_refs=dict:new(),
                                 class_refs=dict:new()}),
    list_to_binary(lists:reverse(State1#state.output_buffer)).

-spec(decode/1 :: (binary()) -> {ok, any(), binary()} | {error, atom() | tuple()}).
decode(Binary) ->
    decode(Binary, []).

-spec(decode/2 :: (binary(), [decode_opt()]) -> {ok, any(), binary()} | {error, atom() | tuple()}).
decode(Binary, Opts) when is_binary(Binary) ->
    InitialState = #state{complex_refs=array:new(),
                          string_refs=array:new(),
                          class_refs=array:new(),
                          decode_keys_as=proplists:get_value(keys, Opts, existing_atom),
                          decode_bytearray_as=proplists:get_value(byte_array, Opts, binary)},
    try decode_value(Binary, InitialState) of
        {Value, Rest, _State} ->
            {ok, Value, Rest}
    catch
        throw:Error ->
            {error, Error}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(output_value/2 :: (any(), #state{}) -> #state{}).
output_value(undefined, State) ->
    output(?AMF3_UNDEF, State);
output_value(null, State) ->
    output(?AMF3_NULL, State);
output_value(false, State) ->
    output(?AMF3_FALSE, State);
output_value(true, State) ->
    output(?AMF3_TRUE, State);
output_value(Integer, State)
  when is_integer(Integer), Integer >= ?AMF3_INT_MIN, Integer =< ?AMF3_INT_MAX ->
    output([?AMF3_INTEGER, encode_varint(Integer)], State);
output_value(Number, State)
  when is_number(Number); Number =:= nan ->
    output([?AMF3_NUMBER, encode_double(Number)], State);
output_value(String, State)
  when is_binary(String); is_atom(String) ->
    output_string(?AMF3_STRING, String, State);
output_value({string, String}, State)
  when is_list(String); is_binary(String); is_atom(String) ->
    output_string(?AMF3_STRING, String, State);
output_value({byte_array, Binary}, State)
  when is_binary(Binary) ->
    output_complex(?AMF3_BYTEARRAY, Binary, fun(B) -> {byte_size(B), B} end, State);
output_value({xml, XML} = Value, State)
  when is_list(XML); is_binary(XML) ->
    output_complex(?AMF3_XML, Value,
                   fun ({xml, XMLString}) ->
                       Binary = to_binary(XMLString),
                       {byte_size(Binary), Binary}
                   end, State);
output_value({date, Seconds} = Value, State)
  when is_number(Seconds) ->
    output_complex(?AMF3_DATE, Value,
                   fun({date, Secs}) -> {0, encode_double(Secs * 1000)} end,  % convert from sec to msec
                   State);
output_value(List, State)
  when is_list(List) ->
    {_, State1} = new_complex_ref(State),
    output_values(List, output([?AMF3_ARRAY, encode_varint(length(List) bsl 1 bor 1), 1], State1));
output_value({obj, Props}, State)
  when is_list(Props) ->
    output_dynamic_object(<<>>, [], Props, State);
output_value({obj, ClassName, Props}, State)
  when is_list(ClassName), is_list(Props);
       is_atom(ClassName), is_list(Props);
       is_binary(ClassName), is_list(Props) ->
    output_static_object(ClassName, Props, State);
output_value({obj, ClassName, StaticProps, DynamicProps}, State)
  when is_list(ClassName), is_list(StaticProps), is_list(DynamicProps);
       is_atom(ClassName), is_list(StaticProps), is_list(DynamicProps);
       is_binary(ClassName), is_list(StaticProps), is_list(DynamicProps) ->
    output_dynamic_object(ClassName, StaticProps, DynamicProps, State);
output_value(Value, State) ->
    case array:is_array(Value) of
        true ->
            output_value(array:to_list(Value), State);
        false ->
            erlang:error({unsupported_type, Value})
    end.

output_static_object(ClassName, Props, State) ->
    %% Allocate a reference index for the object.
    {_, State1} = new_complex_ref(State),
    
    {Keys, Values} = lists:unzip(Props),

    %% Serialize the class definition, or reference.
    State2 = output_object_header(ClassName, Keys, ?AMF3_CLASS_STATIC, State1),
    
    %% Serialize the property values.
    output_values(Values, State2).

output_dynamic_object(ClassName, StaticProps, DynamicProps, State) ->
    %% Allocate a reference index for the object.
    {_, State1} = new_complex_ref(State),
    
    {Keys, Values} = lists:unzip(StaticProps),

    %% Serialize the class definition, or reference.
    State2 = output_object_header(ClassName, Keys, ?AMF3_CLASS_DYNAMIC, State1),
    
    %% Serialize the property values.
    State3 = output_values(Values, State2),
    
    %% Serialize the dynamic property values.
    output_kv_pairs(DynamicProps, State3).

output_object_header(ClassName, StaticKeys, Flags, State) ->
    output_object_header(#amf3_class{name=ClassName, keys=StaticKeys,
                                     flags=Flags},
                         State).

output_object_header(Class, State) ->
    case class_ref(Class, State) of
        false ->
            Flags = length(Class#amf3_class.keys) bsl 4 bor Class#amf3_class.flags,
            output_strings(Class#amf3_class.keys,
                output_string(Class#amf3_class.name,
                    output([?AMF3_OBJECT, encode_varint(Flags)], add_class_ref(Class, State))));
        Ref ->
            output([?AMF3_OBJECT, encode_varint(Ref bsl 2 bor 2#01)], State)
    end.

output_values(Values, State) ->
    lists:foldl(fun output_value/2, State, Values).

output_kv_pairs(KVPairs, State) ->
    %% Serialize the key/value pairs as untagged string keys with tagged values,
    %% terminated by the empty string.
    output(1, lists:foldl(fun output_kv_pair/2, State, KVPairs)).

output_kv_pair({Key, Value}, State) ->
    output_value(Value, output_string(Key, State)).

output_string(TypeTag, String, State) ->
    output_string(String, output(TypeTag, State)).

output_string(<<>>, State) ->
    %% The empty string is never serialized by reference.
    output(1, State);
output_string(String, State) when is_binary(String) ->
    case string_ref(String, State) of
        false ->
            output(String, output(encode_varint(byte_size(String) bsl 1 bor 1), add_string_ref(String, State)));
        Ref ->
            output(encode_varint(Ref bsl 1), State)
    end;
output_string(String, State) ->
    output_string(to_binary(String), State).

output_strings(Strings, State) ->
    lists:foldl(fun output_string/2, State, Strings).

output_complex(TypeTag, Value, Encoder, State) ->
    case complex_ref(Value, State) of
        false ->
            {V, Data} = Encoder(Value),
            output([TypeTag, encode_varint(V bsl 1 bor 1), Data], add_complex_ref(Value, State));
        Ref ->
            output([TypeTag, encode_varint(Ref bsl 1)], State)
    end.

-spec(to_binary/1 :: (string_like()) -> binary()).

to_binary(String) when is_list(String) ->
    list_to_binary(String);
to_binary(Binary) when is_binary(Binary) ->
    Binary;
to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

output(Bytes, State) ->
    State#state{output_buffer=[Bytes|State#state.output_buffer]}.

add_class_ref(Class, State) ->
    State#state{class_refs=dict:store(Class, State#state.next_class_ref, State#state.class_refs),
                next_class_ref=State#state.next_class_ref + 1}.

class_ref(Class, State) ->
    case dict:find(Class, State#state.class_refs) of
        {ok, Ref} ->
            Ref;
        error ->
            false
    end.

complex_ref(Value, State) ->
    case dict:find(Value, State#state.complex_refs) of
        {ok, Ref} ->
            Ref;
        error ->
            false
    end.

new_complex_ref(State) ->
    {State#state.next_complex_ref,
     State#state{next_complex_ref=State#state.next_complex_ref + 1}}.

add_complex_ref(Value, State) ->
    State#state{complex_refs=dict:store(Value, State#state.next_complex_ref, State#state.complex_refs),
                next_complex_ref=State#state.next_complex_ref + 1}.

string_ref(String, State) ->
    case dict:find(String, State#state.string_refs) of
        {ok, Ref} ->
            Ref;
        error ->
            false
    end.

add_string_ref(String, State) ->
    State#state{string_refs=dict:store(String, State#state.next_string_ref, State#state.string_refs),
                next_string_ref=State#state.next_string_ref + 1}.

-spec(decode_value/2 :: (binary(), #state{}) -> {any(), binary(), #state{}}).
decode_value(<<?AMF3_UNDEF, Rest/binary>>, State) ->
    {undefined, Rest, State};
decode_value(<<?AMF3_NULL, Rest/binary>>, State) ->
    {null, Rest, State};
decode_value(<<?AMF3_FALSE, Rest/binary>>, State) ->
    {false, Rest, State};
decode_value(<<?AMF3_TRUE, Rest/binary>>, State) ->
    {true, Rest, State};
decode_value(<<?AMF3_INTEGER, Rest/binary>>, State) ->
    {I, Rest1} = decode_varint(Rest),
    if
        I >= 16#10000000 ->
            {I - 16#20000000, Rest1, State};
        true ->
            {I, Rest1, State}
    end;
decode_value(<<?AMF3_NUMBER, Rest/binary>>, State) ->
    {N, Rest1} = decode_double(Rest),
    {N, Rest1, State};
decode_value(<<?AMF3_STRING, Rest/binary>>, State) ->
    decode_string(Rest, State);
decode_value(<<TypeTag, Rest/binary>>, State) ->
    {V, Rest1} = decode_varint(Rest),
    case V band 1 of
        0 ->
            %% Reference.
            Ref = V bsr 1,
            {seen_complex(Ref, State), Rest1, State};
        1 ->
            {Ref, State1} = new_complex_ref(State),
            {Value, Rest2, State2} = decode_complex(TypeTag, V bsr 1, Rest1, State1),
            {Value, Rest2, add_seen_complex(Ref, Value, State2)}
    end.

decode_complex(XMLType, XMLLen, Bytes, State)
  when XMLType =:= ?AMF3_XMLDOC; XMLType =:= ?AMF3_XML ->
    <<XMLBin:XMLLen/binary, Rest/binary>> = Bytes,
    {{xml, XMLBin}, Rest, State};
decode_complex(?AMF3_DATE, _, Bytes, State) ->
    {Date, Rest} = decode_double(Bytes),
    Value = {date, Date / 1000},  % convert from msec to sec
    {Value, Rest, State};
decode_complex(?AMF3_ARRAY, DenseMemberCount, Rest, State) ->
    {KVPairs, Rest1, State1} = decode_kv_pairs(Rest, State),
    {DenseMembers, Rest2, State2} = decode_values(Rest1, DenseMemberCount, State1),
    Value = case KVPairs of
        [] ->
            DenseMembers;
        _ ->
            {obj, KVPairs ++ lists:zip(lists:seq(1, DenseMemberCount), DenseMembers)}
    end,
    {Value, Rest2, State2};
decode_complex(?AMF3_OBJECT, V, Rest, State) ->
    {Class, Rest1, State1} = decode_object_header(V bsl 1 bor 1, Rest, State),
    {SealedValues, Rest2, State2} = decode_values(Rest1, length(Class#amf3_class.keys), State1),
    SealedProps = lists:zip(Class#amf3_class.keys, SealedValues),
	%%io:format("decode_complex/4/1=~p~n",[SealedProps]),
	%%lists:sort(SealedProps),
	%%io:format("decode_complex/4/2=~p~n",[SealedProps]),
    if
        Class#amf3_class.flags band ?AMF3_CLASS_EXTERN =:= ?AMF3_CLASS_EXTERN ->
            decode_externalizable(Class#amf3_class.name, Rest2, State2);
        Class#amf3_class.flags =:= ?AMF3_CLASS_DYNAMIC ->
            {DynamicProps, Rest3, State3} = decode_kv_pairs(Rest2, State2),
			%%io:format("decode_complex/4/3=~p~n",[DynamicProps]),
            Value = case Class#amf3_class.name of
                <<>> ->
                    {obj, SealedProps ++ DynamicProps};
                ClassName ->
                    {obj, ClassName, SealedProps, DynamicProps}
            end,
            {Value, Rest3, State3};
        Class#amf3_class.flags =:= ?AMF3_CLASS_STATIC ->
            Value = {obj, Class#amf3_class.name, SealedProps},
            {Value, Rest2, State2}
    end;
decode_complex(?AMF3_BYTEARRAY, Length, Bytes, State)
  when State#state.decode_bytearray_as =:= binary->
    <<Binary:Length/binary, Rest/binary>> = Bytes,
    {Binary, Rest, State};
decode_complex(?AMF3_BYTEARRAY, Length, Bytes, State) ->
    <<Binary:Length/binary, Rest/binary>> = Bytes,
    {{byte_array, Binary}, Rest, State};
decode_complex(_, _, _, _State) ->
    throw(bad_object).

-spec(decode_string/2 :: (binary(), #state{}) -> {binary(), binary(), #state{}}).
decode_string(Bytes, State) ->
    {V, Rest1} = decode_varint(Bytes),
    case V band 1 of
        1 ->
            %% Inline string.
            Length = V bsr 1,
            {String, Rest2} = split_binary(Rest1, Length),
            case String of
                <<>> ->
                    {<<>>, Rest2, State};
                _ ->
                    {String, Rest2, add_seen_string(String, State)}
            end;
        0 ->
            %% Referenced string.
            Ref = V bsr 1,
            {seen_string(Ref, State), Rest1, State}
    end.

-spec(decode_values/3 :: (binary(), non_neg_integer(), #state{}) -> {[any()], binary(), #state{}}).
decode_values(Bytes, Count, State) ->
    decode_values(Bytes, Count, [], State).

decode_values(Bytes, 0, Accum, State) ->
	{lists:reverse(Accum), Bytes, State};
decode_values(Bytes, N, Accum, State) ->
    {Value, Rest1, State1} = decode_value(Bytes, State),
    decode_values(Rest1, N - 1, [Value|Accum], State1).

-spec(decode_strings/3 :: (binary(), non_neg_integer(), #state{}) -> {[binary()], binary(), #state{}}).
decode_strings(Bytes, Count, State) ->
    decode_strings(Bytes, Count, [], State).

decode_strings(Bytes, 0, Accum, State) ->
    {lists:reverse(Accum), Bytes, State};
decode_strings(Bytes, N, Accum, State) ->
    {String, Rest1, State1} = decode_string(Bytes, State),
    decode_strings(Rest1, N - 1, [String|Accum], State1).

-spec(decode_kv_pairs/2 :: (binary(), #state{}) -> {[{binary() | atom(), any()}], binary(), #state{}}).
decode_kv_pairs(Bytes, State) ->
    decode_kv_pairs(Bytes, [], State).

decode_kv_pairs(Bytes, Accum, State) ->
    case decode_string(Bytes, State) of
        {<<>>,  Rest1, State1} ->
			%%{lists:reverse(Accum), Rest1, State1};
			{lists:sort(Accum), Rest1, State1};
        {Key, Rest1, State1} ->
            {Value, Rest2, State2} = decode_value(Rest1, State1),
            decode_kv_pairs(Rest2, [{prepare_key(Key, State), Value}|Accum], State2)
    end.

-spec(decode_object_header/3 :: (integer(), binary(), #state{}) -> {#amf3_class{}, binary(), #state{}}).
decode_object_header(Flags, Bytes, State)
  when Flags band 2 =:= 0 ->
    %% Referenced class.
    {seen_class(Flags bsr 2, State), Bytes, State};
decode_object_header(Flags, Bytes, State) ->
    %% Inline class.
    {ClassName, Rest1, State1} = decode_string(Bytes, State),
    {Keys, Rest2, State2} =
        if
            Flags band ?AMF3_CLASS_EXTERN =:= ?AMF3_CLASS_EXTERN ->
                {[], Rest1, State1};
            true ->
                {KeyList, R1, S1} = decode_strings(Rest1, Flags bsr 4, State1),
                {[prepare_key(Key, S1) || Key <- KeyList], R1, S1}
        end,
    Class = #amf3_class{name=prepare_key(ClassName, State2),
                        flags=Flags band 2#1111,
                        keys=Keys},
    {Class, Rest2, add_seen_class(Class, State2)}.

decode_externalizable("flex.messaging.io.ArrayCollection" = ClassName, Bytes, State) ->
    {Object, Rest1, State1} = decode_value(Bytes, State),
    {{obj, ClassName, [{collection, Object}]}, Rest1, State1};
decode_externalizable("flex.messaging.io.ObjectProxy" = ClassName, Bytes, State) ->
    {Object, Rest1, State1} = decode_value(Bytes, State),
    {{obj, ClassName, [{obj, Object}]}, Rest1, State1};
decode_externalizable("flex.messaging.io.ManagedObjectProxy" = ClassName,
                      <<Count:32/big-unsigned, Rest/binary>>, State) ->
    {Items, Rest1, State1} = decode_values(Rest, Count * 2, State),
    {{obj, ClassName, zip_values(Items)}, Rest1, State1};
decode_externalizable(ClassName, _Bytes, _State) ->
    throw({unsupported_externalizable, ClassName}).

-spec(prepare_key/2 :: (binary(), #state{}) -> atom() | binary() | string()).
prepare_key(<<>>, #state{decode_keys_as=existing_atom}) ->
    <<>>;
prepare_key(Key, #state{decode_keys_as=existing_atom}) ->
    try
        list_to_atom(binary_to_list(Key))
		%%list_to_existing_atom(binary_to_list(Key))
    catch
        error:badarg ->
            Key
    end;
prepare_key(<<>>, #state{decode_keys_as=existing_atom_or_list}) ->
    <<>>;
prepare_key(Key, #state{decode_keys_as=existing_atom_or_list}) ->
    String = binary_to_list(Key),
    try
        list_to_existing_atom(String)
    catch
        error:badarg ->
            String
    end;
prepare_key(Key, #state{decode_keys_as=atom}) ->
    list_to_atom(binary_to_list(Key));
prepare_key(Key, #state{decode_keys_as=list}) ->
    binary_to_list(Key);
prepare_key(Key, #state{decode_keys_as=binary}) ->
    Key.

-spec(zip_values/1 :: (list()) -> [{atom() | binary() | string(), any()}]).
zip_values(Objects) ->
    zip_values(Objects, []).
    
zip_values([], Accum) ->
    lists:reverse(Accum);
zip_values([Key, Value|Rest], Accum) ->
    zip_values(Rest, [{Key, Value}|Accum]).

add_seen_string(String, State) when String =/= <<>> ->
    State#state{string_refs=array:set(State#state.next_string_ref, String, State#state.string_refs),
                next_string_ref=State#state.next_string_ref + 1}.

seen_string(Ref, State) ->
    array:get(Ref, State#state.string_refs).

add_seen_complex(Ref, Value, State) ->
    State#state{complex_refs=array:set(Ref, Value, State#state.complex_refs)}.

seen_complex(Ref, State) ->
    array:get(Ref, State#state.complex_refs).

add_seen_class(Class, State) ->
    State#state{class_refs=array:set(State#state.next_class_ref, Class, State#state.class_refs),
                next_class_ref=State#state.next_class_ref + 1}.

seen_class(Ref, State) ->
    array:get(Ref, State#state.class_refs).

-spec(encode_varint/1 :: (amf3_integer()) -> binary() | byte()).
    
encode_varint(I) when I band 16#7f =:= I ->
    I;
encode_varint(I) when I band 16#3fff =:= I ->
    <<(16#80 bor (I bsr 7)), (I band 16#7f)>>;
encode_varint(I) when I band 16#1fffff =:= I ->
    <<(16#80 bor (I bsr 14)),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#0fffffff =:= I; I < 0, I >= ?AMF3_INT_MIN ->
    <<(16#80 bor (I bsr 22) band 16#ff), (16#80 bor (I bsr 15) band 16#ff),
      (16#80 bor (I bsr 8) band 16#ff), (I band 16#ff)>>.

-spec(decode_varint/1 :: (binary()) -> {integer(), binary()}).
decode_varint(<<0:1, I:7, Rest/binary>>) ->
    {I, Rest};
decode_varint(<<1:1, B1:7, 0:1, B2:7, Rest/binary>>) ->
    {B1 bsl 7 bor B2, Rest};
decode_varint(<<1:1, B1:7, 1:1, B2:7, 0:1, B3:7, Rest/binary>>) ->
    {B1 bsl 14 bor (B2 bsl 7) bor B3, Rest};
decode_varint(<<1:1, B1:7, 1:1, B2:7, 1:1, B3:7, B4, Rest/binary>>) ->
    {B1 bsl 22 bor (B2 bsl 15) bor (B3 bsl 8) bor B4, Rest}.

-spec(encode_double/1 :: (number() | nan) -> binary()).
encode_double(nan) ->
    <<0:1, 4095:11/big, 1:52/big>>;
encode_double(Number) ->
    <<Number:64/big-float>>.

-spec(decode_double/1 :: (binary()) -> {number(), binary()}).
decode_double(Binary) ->
    try
        <<Number:64/big-float, Rest/binary>> = Binary,
        {Number, Rest}
    catch
        exit:_ ->
            <<_:64, R/binary>> = Binary,
            {nan, R}
    end.

-ifdef(eunit).
-include_lib("eunit/include/eunit.hrl").

encode_test_() -> [
    ?_assertMatch(<<0>>, encode(undefined)),
    ?_assertMatch(<<1>>, encode(null)),
    ?_assertMatch(<<2>>, encode(false)),
    ?_assertMatch(<<3>>, encode(true)),

    %% Test integer encoding.
    ?_assertMatch(<<4, 0>>, encode(0)),
    ?_assertMatch(<<4, 1>>, encode(1)),
    ?_assertMatch(<<4, 100>>, encode(100)),
    ?_assertMatch(<<4, 127>>, encode(127)),
    ?_assertMatch(<<4, 129, 0>>, encode(1 bsl 7)),
    ?_assertMatch(<<4, 255, 127>>, encode(1 bsl 14 - 1)),
    ?_assertMatch(<<4, 129, 128, 0>>, encode(1 bsl 14)),
    ?_assertMatch(<<4, 255, 255, 127>>, encode(1 bsl 21 - 1)),
    ?_assertMatch(<<4, 255, 255, 127>>, encode(1 bsl 21 - 1)),
    ?_assertMatch(<<4, 128, 192, 128, 0>>, encode(1 bsl 21)),
    ?_assertMatch(<<4, 191, 255, 255, 255>>, encode(1 bsl 28 - 1)),
    ?_assertMatch(<<4, 255, 255, 255, 255>>, encode(-1)),
    ?_assertMatch(<<4, 255, 255, 255, 254>>, encode(-2)),
    ?_assertMatch(<<4, 192, 128, 128, 0>>, encode(-1 bsl 28)),

    %% These values are just large enough to overflow an int.
    ?_assert(encode(float(1 bsl 28)) =:= encode(1 bsl 28)),
    ?_assert(encode(float(-1 bsl 28 - 1)) =:= encode(-1 bsl 28 - 1)),

    %% Test float encoding.
    ?_assertMatch(<<5, 0, 0, 0, 0, 0, 0, 0, 0>>, encode(0.0)),
    ?_assertMatch(<<5, 63, 240, 0, 0, 0, 0, 0, 0>>, encode(1.0)),
    ?_assertMatch(<<5, 191, 240, 0, 0, 0, 0, 0, 0>>, encode(-1.0)),

    %% Test string encoding.
    ?_assertMatch(<<6, 131, 17, "longlonglong", _/binary>>, encode(list_to_binary(string:copies("long", 50)))),
    ?_assertMatch(<<6, (6 bsl 1 bor 1), "sample">>, encode(sample)),
    ?_assertMatch(<<6, (6 bsl 1 bor 1), "sample">>, encode({string, sample})),
    ?_assertMatch(<<6, (6 bsl 1 bor 1), "sample">>, encode({string, "sample"})),
    ?_assertMatch(<<6, (6 bsl 1 bor 1), "sample">>, encode({string, ["s", ["am", "pl", $e]]})),

    %% Test XML encoding.
    ?_assertMatch(<<11, (6 bsl 1 bor 1), "<xml/>">>, encode({xml, "<xml/>"})),
    ?_assertMatch(<<11, (6 bsl 1 bor 1), "<xml/>">>, encode({xml, <<"<xml/>">>})),
    ?_assertMatch(<<11, 131, 17, "longlonglong", _/binary>>, encode({xml, string:copies("long", 50)})),

    %% Test date encoding.
    ?_assertMatch(<<8, 1, 0, 0, 0, 0, 0, 0, 0, 0>>, encode({date, 0})),
    ?_assertMatch(<<8, 1, 64, 143, 64, 0, 0, 0, 0, 0>>, encode({date, 1})),

    %% Test array encoding.
    ?_assertMatch(<<9, (1 bsl 1 bor 1), 1, 4, 0>>, encode([0])),
    ?_assertMatch(<<9, (3 bsl 1 bor 1), 1, 4, 1, 4, 2, 4, 3>>, encode([1, 2, 3])),

    %% Test object encoding.
    ?_assertMatch(<<10, (0 bsl 4 bor 2#1011), 1, 1>>, encode({obj, []})),
    ?_assertMatch(<<10, (0 bsl 4 bor 2#1011), 1, (4 bsl 1 bor 1), "prop", 4, 100, 1>>, encode({obj, [{prop, 100}]})),
    ?_assertMatch(<<10, (0 bsl 4 bor 2#1011), (5 bsl 1 bor 1), "class", (4 bsl 1 bor 1), "prop", 4, 100, 1>>, encode({obj, "class", [], [{prop, 100}]})),
    ?_assertMatch(<<10, (2 bsl 4 bor 2#1011), (5 bsl 1 bor 1), "class",
                   (5 bsl 1 bor 1), "prop1",
                   (5 bsl 1 bor 1), "prop2",
                   4, 100, 4, 129, 72,
                   (4 bsl 1 bor 1), "dyn1", 4, 130, 44,
                   1>>,
                  encode({obj, <<"class">>, [{prop1, 100}, {prop2, 200}], [{dyn1, 300}]})),
    ?_assertMatch(<<10, (2 bsl 4 bor 2#0011), (5 bsl 1 bor 1), "class",
                   (5 bsl 1 bor 1), "prop1",
                   (5 bsl 1 bor 1), "prop2",
                   4, 100, 4, 101>>,
                  encode({obj, <<"class">>, [{prop1, 100}, {prop2, 101}]}))
].

encode_references_test_() -> [
    % Test string references.
    ?_assertMatch(<<9, (2 bsl 1 bor 1), 1, 6, (6 bsl 1 bor 1), "string", 6, (0 bsl 1)>>,
                  encode([<<"string">>, <<"string">>])),
    ?_assertMatch(<<9, (3 bsl 1 bor 1), 1,
                    6, (6 bsl 1 bor 1), "string",
                    6, (7 bsl 1 bor 1), "string1",
                    6, (0 bsl 1)>>,
                  encode([<<"string">>, <<"string1">>, <<"string">>])),

    % Test that XML references and string references are counted separately.
    ?_assertMatch(<<9, (4 bsl 1 bor 1), 1,
                    6, (6 bsl 1 bor 1), "string",
                    11, (6 bsl 1 bor 1), "string",
                    11, (1 bsl 1),
                    6, (0 bsl 1)>>,
                  encode([<<"string">>, {xml, "string"}, {xml, "string"}, <<"string">>])),

    % Test that the empty string is not counted in the reference index.
    ?_assertMatch(<<9, (6 bsl 1 bor 1), 1,
                    6, (6 bsl 1 bor 1), "string",
                    6, (4 bsl 1 bor 1), "test",
                    6, 1,            % empty string
                    6, (0 bsl 1),    % reference "string"
                    6, (1 bsl 1),    % reference "test"
                    6, 1>>,          % empty string
                  encode([<<"string">>, <<"test">>, <<>>, <<"string">>, <<"test">>, <<>>])),

    % Test string and complex references.
    ?_assertMatch(<<9, (4 bsl 1 bor 1), 1,
                    6, (6 bsl 1 bor 1), "string",  % inline "string"
                    8, 1, 0, 0, 0, 0, 0, 0, 0, 0,  % inline {date, 0.0}
                    6, (0 bsl 1),                  % reference "string"
                    8, (1 bsl 1)>>,                % reference {date, 0.0}
                  encode([<<"string">>, {date, 0}, <<"string">>, {date, 0}])),

    % Test date references.
    ?_assertMatch(<<9, (3 bsl 1 bor 1), 1,
                    8, 1, 0, 0, 0, 0, 0, 0, 0, 0,     % inline {date, 0.0}
                    8, (1 bsl 1),                     % reference {date, 0.0}
                    8, 1, 64, 143, 64, 0, 0, 0, 0, 0>>, % inline {date, 1.0}
                  encode([{date, 0}, {date, 0}, {date, 1}])),

    % Test class references.
    ?_assertMatch(<<9, (2 bsl 1 bor 1), 1,
                    10, (0 bsl 4 bor 2#1011), 1, 1,
                    10, (0 bsl 4 bor 1),
                    (5 bsl 1 bor 1), "prop1", 4, 0, 1>>,
                  encode([{obj, []}, {obj, [{prop1, 0}]}])),

    % Test string references and class references.
    ?_assertMatch(<<9, (2 bsl 1 bor 1), 1,
                    10, (0 bsl 4 bor 2#1011), (5 bsl 1 bor 1), "class", 1,
                    10, (0 bsl 2 bor 1),
                    (0 bsl 1), 4, 0, 1>>,
                  encode([{obj, <<"class">>, [], []}, {obj, <<"class">>, [], [{class, 0}]}]))
].

%% Test that a value is correctly encoded and decoded back to itself.
-define(_assertCodec(Expr), ?LET(X, Expr, ?_assertEqual(X, element(2, decode(encode(X)))))).

decode_test_() -> [
    ?_assertCodec(undefined),
    ?_assertCodec(null),
    ?_assertCodec(false),
    ?_assertCodec(true),

    ?_assertCodec(0),
    ?_assertCodec(1),
    ?_assertCodec(2),
    ?_assertCodec(1 bsl 7 - 1),
    ?_assertCodec(1 bsl 7),
    ?_assertCodec(1 bsl 14 - 1),
    ?_assertCodec(1 bsl 14),
    ?_assertCodec(1 bsl 21 - 1),
    ?_assertCodec(1 bsl 21),
    ?_assertCodec(1 bsl 28 - 1),
    ?_assertCodec(-1),
    ?_assertCodec(-2),

    ?_assertCodec(-1 bsl 28),
    ?_assertCodec(0.0),
    ?_assertCodec(1.0),
    ?_assertCodec(-1.0),
    ?_assertCodec(1.0e308),
    ?_assertCodec(-1.0e308),

    ?_assertCodec(<<"shortstring">>),
    ?_assertCodec(list_to_binary(string:copies("longstring", 50))),

    ?_assertCodec({xml, list_to_binary(string:copies("longxml", 50))}),
    ?_assertCodec({xml, <<"<xml/>">>}),
    ?_assertCodec([<<"1">>, {xml, <<"<xml/>">>}, <<"2">>, {xml, <<"<xml/>">>}]),

    ?_assertCodec({date, 0.0}),
    ?_assertCodec({date, 1736428493.9421}),
    ?_assertCodec([{date, 0.0}, {date, 0.0}]),

    ?_assertCodec([]),
    ?_assertCodec([0]),
    ?_assertCodec([0, 1]),
    ?_assertCodec([<<"abc">>, <<"abc">>, <<"def">>, <<"abc">>]),
    ?_assertCodec(string:copies([<<"longarray">>], 500)),

    ?_assertCodec({obj, []}),
    ?_assertCodec({obj, 'AtomClassName', []}),
    ?_assertCodec({obj, <<"Classname">>, []}),
    ?_assertCodec({obj, <<"Classname">>, [{prop1, 1}, {prop2, 2}, {<<"not_an_atom">>, 3}]}),
    %% Test very large objects.
    ?_assertCodec({obj, <<"Classname">>, [{list_to_binary("seqprop" ++ integer_to_list(I)), I} || I <- lists:seq(1, 500)]}),
    ?_assertCodec({obj, <<"Classname">>, [], [{list_to_binary("seqprop" ++ integer_to_list(I)), I} || I <- lists:seq(1, 500)]}),

    %% Test classdef references.
    ?_assertCodec([{obj, []}, {obj, []}]),
    ?_assertCodec([{obj, []}, {obj, [{<<"1">>, 2}]}]),
    ?_assertCodec([{obj, <<"Classname">>, []}, {obj, []}]),

    %% Test dynamic object.
    ?_assertCodec({obj, <<"Classname">>, [{<<"abc">>, 1}], [{<<"def">>, 2}]})
].

-endif.
