-module(libhotswap_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
%% @doc Start up the application tree, using the application env as the
%%   mechanism for configuration. See libhotswap.app.src for more 
%%   information.
%% @end
start( _StartType, StartArgs ) -> 
    libhotswap_sup:start_link( StartArgs ).

%% @private
%% @doc Clean up after the server has all stopped. We currently have 
%%   nothing to do.
%% @end
stop( _State ) -> ok.

