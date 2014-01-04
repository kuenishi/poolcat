-module(poolcat_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Module, WorkerNum,InitData) ->
    supervisor:start_link(?MODULE, [Name,Module,WorkerNum,InitData]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name,Module,WorkerNum,InitData]) ->
    SupFlags = {one_for_one, 1000, 3600},

    Children = [{make_worker_name(Name,Seq),
                 {poolcat_worker, start_link, [Name,Module,InitData]},
                 permanent, 2000, worker, [poolcat_worker, Module]}
                || Seq <- lists:seq(0, WorkerNum)],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_worker_name(Name, Seq) ->
    Str = io_lib:format("poolcat_workers_~p_~p", [Name, Seq]),
    list_to_atom(lists:flatten(Str)).
