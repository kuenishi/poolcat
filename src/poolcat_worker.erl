%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2014 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(poolcat_worker).

-behaviour(gen_server).

-callback handle_pop(term()) -> ok.

%% -callback handle_dropped(term()) -> ok.

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          mod :: atom(),
          state :: term(),
          qname :: atom()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name,Module) ->
    gen_server:start_link(?MODULE, [Name,Module], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Name,Module]) ->
    {ok, State} = Module:init(),
    {ok, #state{mod=Module,state=State,qname=Name}, 1}.

%% @private
handle_call(stop, _, State) ->
    {stop, normal, State};
handle_call(_, _From, State) ->
    {reply, error, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state{mod=Module,qname=QName,state=SubState0}=State) ->
    {ok,Task} = gen_queue:pop(QName),
    {ok,SubState} = Module:handle_pop(Task,SubState0),
    {noreply, State#state{state=SubState}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
