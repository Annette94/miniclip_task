%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : December 04 2020
%%%-------------------------------------------------------------------

-module(crypto_utils).

-export([pad/1,
              unpad/1,
              block_encrypt_gcm/4,
              block_decrypt_gcm/5,
              block_encrypt_cbc/3,
              block_decrypt_cbc/3
             ]).

-define(BYTE_SIZE_UNIT, 16).

%%------------------------------------------------------------------------------
%% Block Encrypt using gcm mode
%%------------------------------------------------------------------------------

block_encrypt_gcm(Key, IV, AAD, PlainText) ->
    crypto:block_encrypt(aes_gcm, Key, IV, {AAD, PlainText}).

%%------------------------------------------------------------------------------
%% Block Decrypt using gcm mode
%%------------------------------------------------------------------------------

block_decrypt_gcm(Key, IV, AAD, CipherText, CipherTag) ->
    crypto:block_decrypt(aes_gcm, Key, IV, {AAD, CipherText, CipherTag}).

%%------------------------------------------------------------------------------
%% Block Encrypt using cbc mode
%%------------------------------------------------------------------------------

block_encrypt_cbc(Key, IV, PlainText) ->
    crypto:block_encrypt(aes_cbc, Key, IV, PlainText).

%%------------------------------------------------------------------------------
%% Block Decrypt using gcm mode
%%------------------------------------------------------------------------------

block_decrypt_cbc(Key, IV, CipherText) ->
    crypto:block_decrypt(aes_cbc, Key, IV, CipherText).

%%------------------------------------------------------------------------------
%% Pad data using block size
%%------------------------------------------------------------------------------

pad(Data) ->
    Size = erlang:byte_size(Data),
    Padding = ?BYTE_SIZE_UNIT - (Size rem ?BYTE_SIZE_UNIT),
    PaddingBin = binary:copy(<<Padding:8/unit:1>>, Padding),
    <<Data/binary, PaddingBin/binary>>.

%%------------------------------------------------------------------------------
%% Unpad data using block size
%%------------------------------------------------------------------------------

unpad(Data) ->
    DataLen = erlang:byte_size(Data),
    ByteSize = (DataLen-1) * 8,
    <<_RCount:ByteSize, Count:8>> = Data,
    Size = DataLen - Count,
    <<ResultBin:Size/binary, _/binary>> = Data,
    ResultBin.
