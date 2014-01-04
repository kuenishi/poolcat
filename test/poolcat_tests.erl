-module(poolcat_tests).

-export([init/1, handle_pop/2, terminate/2]).

-behaviour(poolcat_worker).

-include_lib("eunit/include/eunit.hrl").

init(_) ->
    {ok, {}}.

handle_pop({back,From}, State) ->
    From ! ?MODULE,
    {ok, State};
handle_pop(noop, State) ->
    {ok, State}.

terminate(_, _) ->
    ok.

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
    {ok, Pid2} = poolcat:create_pool(test2, ?MODULE, 32, initdata),

    Num = 1024,
    [ok = poolcat:push_task(test, noop) ||
        _ <- lists:seq(0, Num)],
    [ok = poolcat:push_task(test2, noop) ||
        _ <- lists:seq(0, Num)],

    {ok, []} = poolcat:safe_destroy_pool(test, Pid),
    {ok, []} = poolcat:safe_destroy_pool(test2, Pid2),

    ok = application:stop(poolcat),
    ok = application:stop(gen_queue).

-endif.
