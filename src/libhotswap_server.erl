-module(libhotswap_server).
-behaviour(gen_server).
-include("libhotswap.hrl").

%% API
-export([start_link/1]).
-export([stop/0,stop/1]).
-export([local_instance/0,
         reload/2, reload_all/0,
         get_object_code/1,
         rollback/1, rollback/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Defaults for each of the configurations
-define(D_PERSIST_ON_SHUTDOWN,true).
-define(D_UNLOAD_ON_SHUTDOWN,false).
-define(D_RELOAD_ON_STARTUP,true).
-define(D_ROLLBACK_LENGTH,5).
-define(D_USE_SOFT_PURGE,true).
-define(D_CACHE_DIR,"~/.libhotswap_cache").

%% Local State/Configuration of the system.
-record(state, {
            % Configs
            persist_on_shutdown,
            unload_on_shutdown,
            reload_on_startup,
            rollback_length,
            use_soft_purge,
            cache_dir,
            
            % State
            module_rollback_map = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the HotSwap server. This will keep track of any in-memory changes
%%   and keep on-disk backups if desired.
%% @end
start_link( StartArgs ) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StartArgs], []).

%% @doc Stop the HotSwap Server.
stop() ->
    gen_server:call( ?MODULE, stop ).

%% @doc Stop the HotSwap server and if desired, load the old versions that 
%%   were in place before the server was started.
%% @end
stop( OverrideUnloadOnShutdown ) -> 
    gen_server:call( ?MODULE, {stop, OverrideUnloadOnShutdown} ).

%% @doc Check if a local instance is registered and running, and return it's 
%%   Process Identifier, otherwise return false.
%% @end
local_instance() ->
    case whereis( ?MODULE ) of
        {ok, Pid} -> {ok, Pid};
        _         -> false
    end.

%% @doc Wrap a call to code:get_object_code/1 with versioning and the
%%   metadata required to roll code back.
%% @end
get_object_code( Module ) ->
    gen_server:call( ?MODULE, {get_object_code, Module} ).

%% @doc Will reload a module into memory and save it in the newest position 
%%   for rollback if enabled.
%% @end
reload( Module, Binary ) ->
    gen_server:call( ?MODULE, {reload, Module, Binary} ).

%% @doc On startup by default it will reload all back into memory. However
%%   if this config has been turned off, this function will let the user do
%%   so, at will.
%% @end
reload_all() ->
    gen_server:call( ?MODULE, reload_all ).

%% @doc Rollback the module to the previous saved version in the stack.
rollback( Module ) -> rollback( Module, 1 ).

%% @doc Rollback the module N times. This has the effect of reverting the
%%   last N-1 changes. In otherwords, rollback( test, 1 ), will go to the
%%   previous revision, whereas rollback( test, 2 ) will go to the one
%%   before that, etc.
%% @end
rollback( Module, N ) ->
    gen_server:call( ?MODULE, {rollback, Module, N} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Load up the 
init([StartArgs]) -> 
    case build_init_env() of
        {ok,InitState} -> validate_state(InitState, StartArgs);
        Error          -> Error
    end.

%% @private
%% @doc Handle all API functionality for the libhotswap_server.
handle_call(Request, _From, State) -> 
    case Request of
        {stop, Override} -> 
            handle_call_stop( Override, State );
        stop -> 
            handle_call_stop( State#state.unload_on_shutdown, State );
        {get_object_code, Module } ->
            handle_call_get_object_code( Module, State );
        {reload, Module, Binary } ->
            handle_call_reload( Module, Binary, State );
        reload_all ->
            handle_call_reload_all( State );
        {rollback, Module, N } ->
            handle_call_rollback( Module, N, State );
        Unknown ->
            error( Unknown )
    end.

%%% We leave the following to be default passthroughs.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Build up the initial state by checking the values of the application
%%   environment and using the default values if not present.
%% @end
build_init_env() ->
    #state{
        persist_on_shutdown = application:get_env(libhotswap,persist_on_shutdown,?D_PERSIST_ON_SHUTDOWN),
        unload_on_shutdown = application:get_env(libhotswap,unload_on_shutdown,?D_UNLOAD_ON_SHUTDOWN),
        reload_on_startup = application:get_env(libhotswap,reload_on_startup,?D_RELOAD_ON_STARTUP),
        rollback_length = application:get_env(libhotswap,rollback_length,?D_ROLLBACK_LENGTH),
        use_soft_purge = application:get_env(libhotswap,use_soft_purge,?D_USE_SOFT_PURGE),
        cache_dir = application:get_env(libhotswap,cache_dir,?D_CACHE_DIR)
    }.

%% @hidden
%% @doc Allow passed in application args to mutate the state before completing
%%   initialization.
%% @end
validate_state( #state{cache_dir=CD}=InitState, _StartArgs ) ->
    %TODO: What arguments should we consider as commandline flags?
    ok = verify_cache_dir( CD ),
    State = InitState#state{ module_rollback_map=load_cached_data( CD ) },
    {ok, State}.

%% @hidden
%% @doc Ensure the directory exists for cache storage.
verify_cache_dir( Dir ) -> 
    Path = case filename:split( Dir ) of
               ["~"|Rest] -> filename:join([os:getenv("HOME")]++Rest);
               _          -> Dir
           end,
    filelib:ensure_dir( Path ).

%% @hidden
%% @doc Load the data already in the cache directory for use by the 
load_cached_data( CacheDirectory ) ->
    ok.

%%%===================================================================
%%% Call handlers
%%%===================================================================

%% @hidden
%% @doc On stop, we will return we are shutting down, but before so: we may
%%   need to remove the  
handle_call_stop( UnloadOnShutdown, State ) ->
    case State#state.persist_on_shutdown of
        true  -> ok;
        false -> purge_all( State )
    end,
    case UnloadOnShutdown of
        true  -> unload_all( State );
        false -> ok
    end,
    {stop, normal, ok, State}. 

handle_call_get_object_code( Module, State ) -> ok 
handle_call_reload( Module, Binary, State ) -> ok.
handle_call_reload_all( State ) -> ok.
handle_call_rollback( Module, N, State ) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unload_all( State ) -> ok.
purge_all( State ) -> ok.
