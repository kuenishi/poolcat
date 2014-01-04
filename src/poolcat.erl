-module(poolcat).

-export([create_pool/4, destroy_pool/2,
         safe_destroy_pool/2, push_task/2]).

-include_lib("eunit/include/eunit.hrl").

-spec create_pool(Name::atom(), Module::atom(),
                  Workernum::non_neg_integer(), InitData::any()) ->
                         {ok, pid()}.
create_pool(Name, Module, Workernum, InitData) when is_atom(Name),
                                                    is_atom(Module),
                                                    is_integer(Workernum), Workernum > 0 ->
    %% open a queue
    {ok, _Pid0} = gen_queue:create_queue(Name),
    %% create a pool
    %% ChildSpec = {Name,
    %%              {poolcat_worker_sup, start_link, [Name, Module, Workernum]},
    %%              permanent, 5000, supervisor, [poolcat_worker_sup]},
    %% ok = supervisor:check_childspecs([ChildSpec]),
    ChildSpec = [Name,Module,Workernum,InitData],
    case supervisor:start_child(poolcat_sup, ChildSpec) of
        {ok, Pid1} = Ret when is_pid(Pid1) ->
            Ret;
        Other ->
            {ok, _} = gen_queue:destroy_queue(Name),
            Other
    end.

-spec destroy_pool(Name::atom(), pid()) -> ok.
destroy_pool(Name, Pid) ->
    %% close the queue
    {ok, Data} = gen_queue:destroy_queue(Name),
    %% destroy pool
    ok = supervisor:terminate_child(poolcat_sup, Pid),
    {ok, Data}.

%% @doc ensure all task processed
safe_destroy_pool(Name, Pid) ->
    NumChildren = count_children(Pid),
    [gen_queue:push(Name, {stop, self()}) || _ <- lists:seq(0, NumChildren)],
    [receive {ok, poolcat_worker} -> ok end || _ <- lists:seq(0, NumChildren)],
    ok = supervisor:terminate_child(poolcat_sup, Pid),
    {ok, []} = gen_queue:destroy_queue(Name).
            
count_children(Pid) ->
    proplists:get_value(active, supervisor:count_children(Pid)).

-spec push_task(Name:: atom(), Task::term()) -> ok.
push_task(Name, Task) ->
    gen_queue:push(Name, {task, Task}).
