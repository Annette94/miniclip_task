%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : December 04 2020
%%%-------------------------------------------------------------------

-module(dynamodbtask_requests).

-export([handle_request/5]).

-include("defines.hrl").
-include("dynamodbtask_pb.hrl").

handle_request(set_request_t, #req_envelope{set_req = #set_request{req = #data{key = Key, value = Value}}}, AWSConfig, _CiphertextBlob, PlainTextKey) ->
    PaddedValue = crypto_utils:pad(Value),
    {CipherText,Tag} = crypto_utils:block_encrypt_gcm(base64:decode(PlainTextKey), ?IV, <<>>, PaddedValue),% encrypts the data, gcm mode
    Response = 
        case catch erlcloud_ddb2:put_item(?TABLE_NAME, [{?KEY_FIELD, {s, Key}}, {?VALUE_FIELD_1, {b, CipherText}},{?VALUE_FIELD_2, {b,Tag}}], [], AWSConfig) of
            {ok, []} ->
                #set_response{error = ok};
            Error ->
                lager:error("Error while inserting in DB, Reason : ~p",[Error]),
                #set_response{error = internal}
        end,
    #req_envelope{type = set_response_t, set_resp = Response};
handle_request(get_request_t, #req_envelope{get_req = #get_request{key = Key}}, AWSConfig, _CiphertextBlob, PlainTextKey) ->
    Response = 
        case erlcloud_ddb2:get_item(?TABLE_NAME, {?KEY_FIELD, binary_to_list(Key)}, [], AWSConfig) of
            {ok, []} ->
                #get_response{error = not_found};
            {ok,Value} ->
                EncryptedValue = proplists:get_value(<<"dval">>,Value),
                Tag = proplists:get_value(<<"dtag">>,Value),
                DecryptedValue = crypto_utils:block_decrypt_gcm(base64:decode(PlainTextKey), ?IV, <<>>, EncryptedValue,Tag),%decrypts the data
                UnpaddedValue = crypto_utils:unpad(DecryptedValue),
                #get_response{error = ok, req = #data{key = Key, value = UnpaddedValue}};
            Error->
                lager:error("Error while fetching from DB, Reason : ~p",[Error]),
                #get_response{error = internal}
        end,
    #req_envelope{type = get_response_t, get_resp = Response}.