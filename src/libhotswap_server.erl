-module(libhotswap_server).
-behaviour(gen_server).
-include("libhotswap.hrl").
%% API
-export([start_link/2]).
-export([stop/1]).
-export([reload/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the HotSwap server. This will keep track of any in-memory changes
%%   and keep on-disk backups if desired.
%% @end
-spec start_link( cache_config(), replacement_config() ) -> {ok, pid()} | 
                                                            {error, term()}.
start_link( CacheConfig, ReplacementConfig ) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
                          [CacheConfig, ReplacementConfig], []).


%% @doc Stop the HotSwap server and if desired, load the old versions that 
%%   were in place before the server was started.
%% @end
stop( LoadOldVersions ) -> gen_server:call( {stop, LoadOldVersions} ).


%% @doc Load a new version of a module (MFA) given a new AST. This might be
%%   ripe for improvement (such as with an ondisk cache of all updated 
%%   versions for easy rollback or analysis.
%% @end
-spec reload( mfa(), ast(), boolean() ) -> {ok, vsn()} | {error, atom()}.
reload( {Module,_,_}, AST, HardPurge ) -> 
    {ok, NewBinary} = libhotswap_util:ast_to_beam( AST ),
    PurgeResult = case HardPurge of
                     true  -> code:purge( Module );
                     false -> code:soft_purge( Module )
                  end,
    case PurgeResult of
        false -> {error, not_purged};
        true  -> %TODO: Should use code_server, and wrap with a local disk
                 % cache for all updated modules. This would aid in rollback. 
            (case erlang:load_module( Module, NewBinary ) of
                {module,_} -> vsn( Module );
                Error      -> Error
             end)
    end.  

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Load up the 
init([CacheConfig,ReplacementConfig]) -> 
    case validate_config( CacheConfig, ReplacementConfig ) of
        ok -> {ok, load_state( CacheConfig, ReplacementConfig )};
        Error -> Error
    end.


handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

