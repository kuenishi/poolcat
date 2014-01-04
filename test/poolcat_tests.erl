-module(poolcat_tests).

-compile(export_all).
-behaviour(poolcat_worker).

-include_lib("eunit/include/eunit.hrl").

init(_) ->
    {ok, {}}.

handle_pop({back,From}, State) ->
    From ! ?MODULE,
    {ok, State};
handle_pop(noop, State) ->
    {ok, State}.

-ifdef(TEST).

start_stop_test() ->
    ok = application:start(gen_queue),
    ok = application:start(poolcat),

    {ok, Pid} = poolcat:create_pool(test, ?MODULE, 1, initdata),

    ok = poolcat:push_task(test, {back,self()}),
    receive ?MODULE -> ok;
            Other -> ?debugVal(Other), ?assert(false)
    after 1024 -> ?assert(false) end,

    {ok, []} = poolcat:safe_destroy_pool(test, Pid),

    ok = application:stop(poolcat),
    ok = application:stop(gen_queue).

multi_test() ->
    ok = application:start(gen_queue),
    ok = application:start(poolcat),

    {ok, Pid} = poolcat:create_pool(test, ?MODULE, 1, initdata),

    Num = 1024,
    [ok = poolcat:push_task(test, noop) ||
        _ <- lists:seq(0, Num)],
    {ok, []} = poolcat:safe_destroy_pool(test, Pid),
    ok = application:stop(poolcat),
    ok = application:stop(gen_queue).

-endif.
