%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2014 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(poolcat).

-export([create_pool/3, destroy_pool/1, push_task/2]).

-spec create_pool(Name::atom(), Module::atom(), Workernum::non_neg_integer()) -> {ok, pid()}.
create_pool(Name, Module, Workernum) ->
    %% open a queue
    {ok, _Pid0} = gen_queue:create_queue(Name),
    %% create a pool
    ChildSpec = {Name,
                 {poolcat_worker_sup, start_link, [Name, Module, Workernum]},
                 permanent, 5000, supervisor, [poolcat_worker_sup]},
    case supervisor:start_child(poolcat_sup, ChildSpec) of
        {ok, Pid1} = Ret when is_pid(Pid1) ->
            Ret;
        Other ->
            {ok, _} = gen_queue:destroy_queue(Name),
            Other
    end.

-spec destroy_pool(Name::atom() | pid()) -> ok.
destroy_pool(Name) ->
    %% close the queue
    {ok, Data} = gen_queue:destroy_queue(Name),
    %% destroy pool
    ok = poolcat_worker_sup:stop_workers(Name),
    {ok, Data}.

-spec push_task(Name:: atom(), Task::term()) -> ok.
push_task(Name, Task) ->
    gen_queue:push(Name, {task, Task}).
