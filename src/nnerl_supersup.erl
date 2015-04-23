-module(nnerl_supersup).
-behaviour(supervisor).
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).
 
start_link() ->
    supervisor:start_link({local, nnerl}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
stop() ->
    case whereis(nnerl) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                    {nnerl_sup, start_link, [Name, Limit, MFA]},
                        permanent, 10500, supervisor, [nnerl_sup]},
    supervisor:start_child(nnerl, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(nnerl, Name),
    supervisor:delete_child(nnerl, Name).
