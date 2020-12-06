%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : December 03 2020
%%%-------------------------------------------------------------------

-module(dynamodbtask_client).

-behaviour(gen_server).

-include("dynamodbtask_pb.hrl").
-include("defines.hrl").

-export([start_link/0,
              stop/0,
              set_request/2,
              get_request/1]).

-export([init/1,
             handle_call/3,
             handle_cast/2,
             handle_info/2,
             terminate/2,
             code_change/3]).

-record(state, {socket}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_request(Key, Value) ->
    gen_server:call(?MODULE, {set_request, Key, Value}).

get_request(Key) ->
    gen_server:call(?MODULE, {get_request, Key}).

stop() -> gen_server:stop(?MODULE).
    
%%------------------------------------------------------------------------------
init([]) ->
    {ok, Socket} = gen_tcp:connect("localhost", ?PORT, [binary, {active, false}, {packet, 0},{keepalive,true}, {recbuf, 99999}], infinity),% Change the server's IP if needed
    {ok, #state{socket = Socket}}.

handle_call({set_request, Key, Value}, _From, #state{socket = Socket} = State) ->
    Request =  #set_request{req = #data{key = Key, value = Value}},
    Binary_Request = dynamodbtask_pb:encode_msg(#req_envelope{type = set_request_t, set_req = Request}),
    ok = gen_tcp:send(Socket, base64:encode(Binary_Request)),
    case recv_resp(Socket) of
       {error, Error} ->  
            gen_tcp:close(Socket),
            {stop, closed, {error, Error}, State};
        Response -> 
            Reply = decode_message(Response),
            {reply, Reply, State}
    end;
handle_call({get_request, Key}, _From, #state{socket = Socket} = State) ->
    Request = #get_request{key = Key},
    Binary_Request = dynamodbtask_pb:encode_msg(#req_envelope{type = get_request_t, get_req = Request}),
    ok = gen_tcp:send(Socket, base64:encode(Binary_Request)),
    case recv_resp(Socket) of
        {error, Error} ->  
            gen_tcp:close(Socket),
            {stop, closed, {error, Error}, State};
        Response -> 
            Reply = decode_message(Response),
            {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tcp_closed, State) ->
    gen_tcp:close(State#state.socket),
    {stop, closed, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
recv_resp(Socket) ->
    case gen_tcp:recv(Socket, 0, 3000) of
        {ok, Data} -> Data;
        Error -> Error
    end.

decode_message(Msg) ->
    DecodedMsg = dynamodbtask_pb:decode_msg(Msg, req_envelope),
    process_message(DecodedMsg).

process_message(#req_envelope{type = set_response_t, set_resp = #set_response{error = Error}}) -> Error;

process_message(#req_envelope{type = get_response_t, get_resp = #get_response{error = ok, req = #data{value = Value}}}) -> Value;

process_message(#req_envelope{type = get_response_t, get_resp = #get_response{error = Error}}) -> {error, Error}.
