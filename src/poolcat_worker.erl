-module(poolcat_worker).

-behaviour(gen_server).

-callback init(any()) -> {ok, term()}.
-callback handle_pop(term(), term()) -> ok.
%% -callback handle_dropped(term(), term()) -> ok.
%% -callback terminate(term()) -> ok.

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

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
start_link(Name,Module,InitData) ->
    gen_server:start_link(?MODULE, [Name,Module,InitData], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Name,Module,InitData]) ->
    {ok, State} = Module:init(InitData),
    {ok, #state{mod=Module,state=State,qname=Name}, 1}.

%% @private
handle_call(stop, _, State) ->
    {stop, normal, State};
handle_call(_, _From, State) ->
    {reply, error, State, 0}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State, 0}.

%% @private
handle_info(timeout, #state{mod=Module,qname=QName,state=SubState0}=State) ->
    case gen_queue:pop(QName) of
        {ok,{task, Task}} ->
            {ok,SubState} = Module:handle_pop(Task,SubState0),
            {noreply, State#state{state=SubState}, 0};
        {ok, stop} ->
            {stop, normal, SubState0};
        {ok, {stop, Pid}} ->
            Pid ! {ok, ?MODULE},
            {stop, normal, SubState0};
        {error, destroyed} ->
            {stop, normal, SubState0}
    end.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
