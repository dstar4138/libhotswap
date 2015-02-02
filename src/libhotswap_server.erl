-module(libhotswap_server).
-behaviour(gen_server).
-include("libhotswap.hrl").

%% API
-export([start_link/0]).
-export([stop/0,stop/1]).
-export([local_instance/0,
         hotswap/2, hotswap_all/0,
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
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
hotswap( Module, Binary ) ->
    gen_server:call( ?MODULE, {hotswap, Module, Binary} ).

%% @doc On startup by default it will reload all back into memory. However
%%   if this config has been turned off, this function will let the user do
%%   so, at will.
%% @end
hotswap_all() ->
    gen_server:call( ?MODULE, hotswap_all ).

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
init([]) -> 
    case build_init_env() of
        {ok,InitState} -> validate_state( InitState );
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
        {hotswap, Module, Binary } ->
            handle_call_hotswap( Module, Binary, State );
        hotswap_all ->
            handle_call_hotswap_all( State );
        {rollback, Module, N } ->
            handle_call_rollback( Module, N, State );
        Unknown ->
            error( Unknown )
    end.

%%% Leaving the following to be default passthroughs.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Initialization functions
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
validate_state( #state{cache_dir=CD, reload_on_startup=Reload}=InitState ) ->
    ok  = verify_cache_dir( CD ),
    Map = load_all_cache_files( CD ),
    case Reload of
        true  -> hotswap_all( Map );
        false -> ok
    end,
    {ok, InitState#state{ module_rollback_map=Map }}.

%% @hidden
%% @doc Ensure the directory exists for cache storage.
verify_cache_dir( Dir ) -> 
    Path = case filename:split( Dir ) of
               ["~"|Rest] -> filename:join([os:getenv("HOME")]++Rest);
               _          -> Dir
           end,
    filelib:ensure_dir( Path ).

%% ===================================================================
%% Call handlers
%% ===================================================================

%% @hidden
%% @doc On stop, we will return we are shutting down, but before so: we may
%%   need to remove all the cached files, and revert all modules to their 
%%   original state.
%% @end
handle_call_stop( UnloadOnShutdown, #state{module_rollback_map=Map}=State ) ->
    NewMap = case State#state.persist_on_shutdown of
        true  -> Map;
        false -> purge_all( State ), []
    end,
    case UnloadOnShutdown of
        true  -> unload_all( State );
        false -> ok
    end,
    {stop, normal, ok, State#state{module_rollback_map=NewMap}}. 

%% @hidden
%% @doc Get the currently loaded module code, and mimic code:get_object_code/1.
handle_call_get_object_code( Mod, State=#state{ cache_dir=CD, 
                                                module_rollback_map=Map } ) ->
    case proplists:get_value( Mod, Map ) of
       undefined -> % If not found, go to code server. 
            DefaultRet = code:get_object_code( Mod ),
            {reply, DefaultRet, State};
       [{_,Binary}|_] -> % Grab top version on the stack. 
            {reply, {Mod,Binary,CD}, State}
    end. 

%% @hidden
%% @doc Perform a hotswap given a module and it's new binary. This will create
%%   a new cached file, and remove any that fall off the end of the rollback
%%   length.
%% @end
handle_call_hotswap( Module, Binary, State=#state{ cache_dir=CD,
                                                  rollback_length=Max,
                                                  use_soft_purge=SoftPurge,
                                                  module_rollback_map=Map } ) ->
    NewEntry = {beam_lib:version(Binary), Binary},
    NextVsn  = next_vsn( Module, Map ),
    ok = create_cache_file( CD, Module, NextVsn, Binary ),
    case proplists:get_value( Module, Map ) of
        undefined -> % Add binary to rollback server and load binary into memory
            case libhotswap_util:reload( Module, Binary, SoftPurge ) of
                ok ->
                    NewMod = {Module, [NewEntry]},
                    NewState = State#state{module_rollback_map=[NewMod|Map]}, 
                    {reply, ok, NewState};
                error ->
                    {reply, error, State}
            end;
        Stack ->
            case libhotswap_util:reload( Module, Binary, SoftPurge ) of
                ok ->
                    {Save, PurgeSet} = lists:split( Max, [NewEntry|Stack] ),
                    ok = purge_cache_files( CD, Module, PurgeSet ),
                    NewMap = lists:keyreplace( Module, 1, {Module,Save}, Map ),
                    NewState = State#state{module_rollback_map=NewMap},
                    {reply, ok, NewState};
                error ->
                    {reply, error, State}
            end
    end.

%% @hidden
%% @doc Perform a reload of all modules in the cache to their "newest" 
%%   versions. This is performed on startup by default, unless turned off.
%% @end
handle_call_hotswap_all( State ) -> 
    {reply, hotswap_all( State ), State}.
    
%% @hidden
%% @doc Roll a module back and reload it into memory. This has the effect
%%   of removing all rolled-back cached data as well (i.e. you can't 
%%   unrollback).
%% @end
handle_call_rollback( Module, N, State=#state{ cache_dir=CD,
                                               rollback_length=Max,
                                               use_soft_purge=SoftPurge,
                                               module_rollback_map=Map } )
                                         when is_integer( N ) andalso N >= 0 ->
    case proplists:get_value( Module, Map ) of
        undefined -> {reply, {error, missing_module}, State};
        Stack ->
            case length(Stack) >= Max of
                true -> 
                    ok = purge_cache_files( CD, Module, Stack ),
                    case code:get_object_code( Module ) of
                        {_, Binary, _} -> 
                            NewMap = lists:keyreplace( Module, 1, {Module,[]}, Map ),
                            NewState = State#state{module_rollback_map=NewMap},
                            Return = libhotswap_util:reload( Module, Binary, SoftPurge ),
                            {reply, Return, NewState};
                        error -> 
                            {reply, {error, bad_module}, State}
                    end;
                false ->
                    {Top, [{_,Binary}|_]=NewStack} = lists:split( N-1, Stack ),
                    ok = purge_cache_files( CD, Module, Top ),
                    NewMap = lists:keyreplace( Module, 1, {Module,NewStack}, Map ),
                    NewState = State#state{module_rollback_map=NewMap},
                    Return = libhotswap_util:reload( Module, Binary, SoftPurge ),
                    {reply, Return, NewState}
            end
    end.

%% ===================================================================
%%  Cache modifications
%% ===================================================================

%% @hidden
%% @doc Load the data already in the cache directory for use by the local
%%   libhotswap code server wrapper. This cache will have multiple rollback
%%   versions in memory for swap out. This will not override them on the code
%%   server, just load them into the wrapper's memory. 
%% @end 
load_all_cache_files( CacheDirectory ) ->
    RegEx = ".*\.beam", % Scan over just BEAM files
    Recursive = true,   % Sure, just check everything if the user messed around
    Init = [],          % Proplist state [ {Module, [{Vsn,Beam}|_]} | _ ] 
    Fun = fun( BeamFile, Map ) ->
                 case load_cache_file( BeamFile ) of
                    {Module, Beam, _Dir} -> 
                        Stack = proplists:get_value( Module, Map, [] ),
                        Vsn = filename_version( BeamFile ),
                        BackwardsStack = lists:sort ([{Vsn,Beam}|Stack] ),
                        NewStack = lists:reverse( BackwardsStack ),
                        lists:keyreplace( Module, 1, {Module, NewStack}, Map );
                    error -> Map
                 end
          end,
    filelib:fold_files( CacheDirectory, RegEx, Recursive, Fun, Init ).

%% @hidden
%% @doc Load the beam file from disk in the same way code:get_object_code/1
%%   works (i.e. keep return type signature similar).
%% @end
load_cache_file( BeamFile ) -> 
    case file:read_file( BeamFile ) of
        {ok, Beam} -> 
            Info = beam_lib:info( Beam ),
            case proplists:get_value( module, Info ) of
                undefined -> {error, undefined};
                Module -> {Module, Beam, filename:dirname(BeamFile)}
            end;
        Error -> Error
    end.

%% @hidden
%% @doc Purge a set of beam files from the on-disk cache.
purge_cache_files( _, _, [] ) -> ok;
purge_cache_files( CD, Module, [{Vsn,_Binary}|R] ) ->
    FilePath = lists:flatten( [CD,"/",atom_to_list(Module),
                                "_",integer_to_list(Vsn),".beam"] ),
    file:delete( FilePath ), % Intentional ignore return type.
    purge_cache_files( CD, Module, R ).


%% @hidden
%% @doc Create a cache file on 
create_cache_file( CD, Module, Vsn, Binary ) ->
    FilePath = lists:flatten( [CD,"/",atom_to_list(Module),
                                "_",integer_to_list(Vsn),".beam"] ),
    file:write_file( FilePath, Binary ).     

%% ===================================================================
%% Code Server modifications
%% ===================================================================

%% @hidden
%% @doc 
unload_all( #state{use_soft_purge=SoftPurge, module_rollback_map=Map} ) ->
    Unload = fun( {Module,_} ) ->
                case code:get_object_code( Module ) of
                    {_Mod, Binary, _Dir} -> 
                        libhotswap_util:reload( Module, Binary, SoftPurge );
                    _ -> ignore
                end
             end,
    lists:foreach( Unload, Map ).

%% @hidden
%% @doc Loop through all cached modules and reload all of their newest versions
%%   into the code server and perform a soft or hard purge of the current 
%%   version loaded. Typically done when the server is started up.
%% @end
hotswap_all( #state{use_soft_purge=SoftPurge, module_rollback_map=Map} ) ->
    PerformSwap = fun( {Module, RollbackStack} ) ->
                          case RollbackStack of
                              [{_Vsn,Binary}|_] ->
                                libhotswap_util:reload(Module,Binary,SoftPurge);
                              _ -> ignore
                          end
                  end,
    lists:foreach( PerformSwap, Map ).

%% @hidden
%% @doc Loop through all cached files and delete them from the on-disk 
%%   directory.
%% @end
purge_all( #state{cache_dir=CD, module_rollback_map=Map} ) ->
    Purge = fun( {Module, RollbackStack} ) ->
                    purge_cache_files( CD, Module, RollbackStack )
            end,
    lists:foreach( Purge, Map ).

%% ===================================================================
%% Local Utility Functionality
%% ===================================================================

%% @hidden
%% @doc We save cache files like: module_version.beam. This returns the 
%%   version as an integer.
%% @end
filename_version( Filename ) ->
    Name = filename:rootname( filename:basename( Filename ) ),
    case string:tokens( Name, "_" ) of
        [_Module,Version] -> 
            case string:to_integer( Version ) of
                {Int,_} -> Int;
                _ -> 0
            end;
        _ -> 0
    end.

%% @hidden
%% @doc We create a new internal version for saving to the cache.
next_vsn( Module, Map ) ->
    case proplists:get_value( Module, Map ) of
        undefined -> 0;
        [] -> 0;
        [{Vsn,_}|_] -> Vsn+1
    end.

