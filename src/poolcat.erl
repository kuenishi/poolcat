-module(poolcat).

-export([create_pool/4, destroy_pool/2,
         safe_destroy_pool/2,
         safe_destroy_pool/3,
         push_task/2,
         sync_exec_task/2, async_exec_task/3,
         status/1,
         pause/1,
         resume/1]).

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

%% TODO: don't use Pid, but make some reference
%% from queue name to supervisor pid, or vice versa
-spec destroy_pool(Name::atom(), pid()) -> ok.
destroy_pool(Name, Pid) ->
    %% close the queue
    {ok, Data} = gen_queue:destroy_queue(Name),
    %% destroy pool
    ok = supervisor:terminate_child(poolcat_sup, Pid),
    {ok, Data}.

%% @doc ensure all task processed
safe_destroy_pool(Name, Pid) ->
    safe_destroy_pool(Name, Pid, fun(_) -> ok end).

safe_destroy_pool(Name, Pid, ProgressIndicator)
  when is_function(ProgressIndicator) ->
    NumChildren = count_children(Pid),
    %% [gen_queue:push(Name, {stop, self()}) || _ <- lists:seq(0, NumChildren)],
    %% [receive {ok, poolcat_worker} -> ok end || _ <- lists:seq(0, NumChildren)],

    Status = status(Name),
    case {proplists:get_value(length,Status),
          proplists:get_value(waiting,Status)} of
        {0, NumChildren} ->
            {ok, []} = destroy_pool(Name, Pid);
        Progress ->

            catch ProgressIndicator(Progress),

            timer:sleep(1024),
            safe_destroy_pool(Name, Pid, ProgressIndicator)
    end.

count_children(Pid) ->
    proplists:get_value(active, supervisor:count_children(Pid)).

-spec push_task(Name:: atom(), Task::term()) -> ok.
push_task(Name, Task) ->
    gen_queue:push(Name, {task, Task}).

-spec sync_exec_task(Name::atom(), Task::term()) -> ok.
sync_exec_task(Name, Task) ->
    TaskID = make_ref(),
    case async_exec_task(Name, Task, TaskID) of
        ok ->
            receive
                {TaskID, StateResult} -> StateResult;
                _Other -> {error, _Other}
            end;
        Error ->
            Error
    end.

-spec async_exec_task(Name::atom(), Task::term(), TaskID::term()) -> ok.
async_exec_task(Name, Task, TaskID) ->
    Self = self(),
    ok = gen_queue:push(Name, {task, Task, TaskID, Self}).

-spec status(Name::atom() | pid()) -> proplists:proplist().
status(Name) ->
    gen_queue:info(Name).

%% TODO: don't use Pid, but make some reference
%% from queue name to supervisor pid, or vice versa
-spec pause(pid()) -> ok.
pause(Pid) when is_pid(Pid) ->
    lists:foreach(fun({_Id, Child, _Type, _Modules}) when is_pid(Child) ->
                          gen_server:cast(Child, pause);
                     (_) ->
                          ok
                  end, supervisor:which_children(Pid)).

-spec resume(pid()) -> ok.
resume(Pid) when is_pid(Pid) ->
    lists:foreach(fun({_Id, Child, _Type, _Modules}) when is_pid(Child) ->
                          gen_server:cast(Child, resume);
                     (_) ->
                          ok
                  end, supervisor:which_children(Pid)).
