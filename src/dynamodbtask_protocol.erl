%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : December 04 2020
%%%-------------------------------------------------------------------

-module(dynamodbtask_protocol).

-behaviour(ranch_protocol).

-include("dynamodbtask_pb.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok,Pid}.

init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{recbuf, 99999},{active, false}, binary]),
    loop(Socket, Transport, Opts).

%%-----------------------------------------------------------------------------------------------
%% Recieves the Payload, encodes it and does the Dynomodb insert/retrieve
%%------------------------------------------------------------------------------------------------

loop(Socket, Transport, [AWSConfig, CiphertextBlob, Key]) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Msg} ->
            #req_envelope{type = Type} = Request = dynamodbtask_pb:decode_msg(base64:decode(Msg), req_envelope),
            Response = dynamodbtask_requests:handle_request(Type, Request, AWSConfig, CiphertextBlob, Key),
            EncodedResponse = dynamodbtask_pb:encode_msg(Response),
            Transport:send(Socket, EncodedResponse),       
            loop(Socket, Transport,[AWSConfig,CiphertextBlob, Key]);
        {error, timeout} ->  
            timer:sleep(20), % Try again after a small sleep
            loop(Socket, Transport,[AWSConfig, CiphertextBlob, Key]);
        _ ->
            ok = Transport:close(Socket)
    end.
