-module(nnerl_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).%% The friendly supervisor is started dynamically!
-define(SPEC(MFA),
    {worker_sup,
        {ppool_worker_sup, start_link, [MFA]},
        temporary,
        10000,
        supervisor,
        [ppool_worker_sup]}).

alloc(Chs) ->
    gen_server:call(ch3, alloc).

free(Ch, Chs) ->
    gen_server:cast(ch3, {free, Ch}).

channels() ->
    ok.

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).
 
start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).
 
run(Name, Args) ->
    gen_server:call(Name, {run, Args}).
 
sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).
 
async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).
 
stop(Name) ->
    gen_server:call(Name, stop).

init(_Args) ->
    {ok, channels()}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_info({'EXIT', Pid, Reason}, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(shutdown, State) ->
    ok.