%%%-------------------------------------------------------------------
%%% @author Annette Jebastina
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : November 27 2020
%%%-------------------------------------------------------------------

-module(dynamodbtask_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.	
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.