-behaviour(application).
-export([start/2, stop/1, start_pool/3,
        run/2, sync_queue/2, async_queue/2, stop_pool/1]).
 
start(normal, _Args) ->
    ppool_supersup:start_link().
 
stop(_State) ->
    ok.