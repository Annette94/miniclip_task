%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : December 04 2020
%%%-------------------------------------------------------------------

-module(dynamodbtask_app).

-behaviour(application).

-include("defines.hrl").

-export([
                start/2,
                stop/1,
                prepare_aws_kms/0,
                create_dynamodb_table/0
            ]).

%%------------------------------------------------------------------------------
%% Start Application
%%------------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    application:ensure_all_started(erlcloud),
    application:set_env(erlcloud, aws_access_key_id, ?ACCESS_KEY_ID),
    application:set_env(erlcloud, aws_secret_access_key, ?SECRET_ACCCESS_KEY),

    create_dynamodb_table(),
    {AWSConfig, CiphertextBlob, PlainTextKey} = prepare_aws_kms(),

    {ok, _} = ranch:start_listener(?LISTENER_NAME, ?TRANSPORT, 
                                                   #{socket_opts => [{port, ?PORT}], 
                                                   max_connections => ?MAX_CONNECTIONS,
                                                   num_acceptors => ?ACCEPTORS_COUNT},
                                                   dynamodbtask_protocol, [AWSConfig, CiphertextBlob, PlainTextKey]),

    lager:info("Service running on port : ~p", [?PORT]),

    dynamodbtask_sup:start_link().

%%------------------------------------------------------------------------------
%% Customer Master Key (CMK) is created and alias name is given to the key
%% Data Key is generated using the CMK Key.
%% CipheredKey returned in stored in aws dynamodb.
%%------------------------------------------------------------------------------

prepare_aws_kms() ->
    {ok, AWSConfig} = erlcloud_aws:profile(),
    {ok,Aliases}=erlcloud_kms:list_aliases(),
    AliasesList = proplists:get_value(<<"Aliases">>,Aliases),
    CMKKeyId = 
        case [Alias || Alias <- AliasesList , proplists:get_value(<<"AliasName">>,Alias) == ?ALIAS_NAME] of
            [] ->
                {ok, [{_, CMK}]} = erlcloud_kms:create_key([], AWSConfig),
                KeyId = proplists:get_value(<<"KeyId">>,CMK),
                erlcloud_kms:create_alias(?ALIAS_NAME,KeyId),
                lager:info("CMK key created"),
                KeyId;
            [Value] -> 
                lager:info("Already created CMK key is used"),
                proplists:get_value(<<"TargetKeyId">>,Value)
        end,

    {CiphertextBlob, PlainTextKey} = 
        case erlcloud_ddb2:get_item(?TABLE_NAME, {?KEY_FIELD, "instancedatakey"}, [], AWSConfig) of
            {ok, []} ->
                {ok, KeyDetails} = erlcloud_kms:generate_data_key(CMKKeyId,[{key_spec, 'AES_128'}],AWSConfig),
                CipheredKey = proplists:get_value(<<"CiphertextBlob">>, KeyDetails),
                PlainKey = proplists:get_value(<<"Plaintext">>, KeyDetails),
                lager:info("Data Key created using CMK"),
                erlcloud_ddb2:put_item(?TABLE_NAME, [{?KEY_FIELD, {s, "instancedatakey"}}, {?VALUE_FIELD_1, {b, CipheredKey}}], [], AWSConfig),
                {CipheredKey, PlainKey};
            {ok, DataKey} -> % If data key is already present, get the plainkey using the decrypt function.
                CipheredKey = proplists:get_value(<<"dval">>,DataKey),
                {ok, KMSConfig} = erlcloud_kms:decrypt(CipheredKey, [], AWSConfig),
                PlainKey = proplists:get_value(<<"Plaintext">>,KMSConfig),
                {CipheredKey,PlainKey}
        end,
    {AWSConfig,  CiphertextBlob, PlainTextKey}.

%%------------------------------------------------------------------------------
%% Checks if table 'dynamostore' is already present. If not, creates a new one.
%%------------------------------------------------------------------------------

create_dynamodb_table() ->
    {ok, TableList} = erlcloud_ddb2:list_tables(),
    case lists:member(?TABLE_NAME, TableList) of
        true ->
            lager:info("Table already created in dynamoDB"),
            ok;
         _ -> 
            erlcloud_ddb2:create_table(?TABLE_NAME ,[{?KEY_FIELD, s}], ?KEY_FIELD, [{provisioned_throughput, {?READ, ?WRITE}}]),
            lager:info("Table created in dynamoDB")
    end.

%%------------------------------------------------------------------------------
%% Stop Application
%%------------------------------------------------------------------------------

stop(_State) ->
    ranch:stop_listener(?LISTENER_NAME).
