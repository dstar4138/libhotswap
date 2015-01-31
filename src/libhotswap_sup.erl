-module(libhotswap_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor and server for libhotswap.
start_link( StartArgs ) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ StartArgs ]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Start up the local libhotswap_server.
init( [StartArgs] ) ->
    %TODO: Allow for distributed systems (do we need a server per node?)
    {ok, { {one_for_one, 5, 10}, 
           [ {local_libhotswap, 
                {libhotswap_server, start_link, [StartArgs]}, 
                permanent, 
                5000, 
                worker, 
                [libhotswap_server]}
           ]}}.

