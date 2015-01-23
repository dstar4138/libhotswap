-module( libhotswap_util_tests ).
-include_lib("eunit/include/eunit.hrl").
-define(SETUP( Fs ), {setup, fun() -> [] end, fun(_) -> [] end, Fs}). 

%% ===========================================================================
%% Test Descriptions
%% ===========================================================================
check_unsticky2_test_() ->
    {"Verify sticky directory checking and manipulation.",
        ?SETUP({inparallel, 
                [ fun validate_stdlibs__check_unsticky2/0,
                  fun validate_locallibs__check_unsticky2/0
                ]})
    }.

get_beam1_test_() ->
    {"Verify we are getting the correct/newest loaded BEAM binaries for"
     "any given queried module.",
        ?SETUP({inparallel,
                [ fun validate_correctModule__get_beam1/0
                ]})
    }.

beam_to_ast1_test_() ->
    {"Verify we are getting an AST back for a BEAM module.",
        ?SETUP({inparallel,
                [ 
                ]})
    }. 

%% ===========================================================================
%% Actual Tests
%% ===========================================================================

%% TEST - Validate if forced, a standard lib can be unstuck and return ok.
%% TEST - Validate that if unforced, a standard lib will return error and
%%         the directory will stay stuck.
validate_stdlibs__check_unsticky2() ->
    FUN = fun libhotswap_util:check_unsticky/2,
    MODULE = io,
    DIR = filename:dirname( code:which( MODULE ) ),
    ?assertEqual( true,  code:is_sticky( MODULE ) ),
    ?assertEqual( error, FUN( MODULE, false ) ),
    ?assertEqual( true,  code:is_sticky( MODULE ) ),
    ?assertEqual( ok,    FUN( MODULE, true )),
    ?assertEqual( false, code:is_sticky( MODULE ) ),
    ?assertEqual( ok,    code:stick_dir( DIR ) ). % Leave it like we found it.

%% TEST - Validate that any local libraries (i.e. libhotswap), which are 
%%         typically nonsticky. Are also subject to unstick requests.
validate_locallibs__check_unsticky2() ->
    FUN = fun libhotswap_util:check_unsticky/2,
    MODULE = libhotswap_util,
    ?assertEqual( false, code:is_sticky( MODULE ) ),
    ?assertEqual( ok,    FUN( MODULE, false ) ),
    ?assertEqual( false, code:is_sticky( MODULE ) ),
    ?assertEqual( ok,    FUN( MODULE, true ) ),
    ?assertEqual( false, code:is_sticky( MODULE ) ).


%% TEST - Validate that we are able to attach to code server and pull object 
%%         code correctly.
validate_correctModule__get_beam1() ->
    FUN = fun libhotswap_util:get_beam/1,
    MOD = libhotswap,
    {RESULT, BEAM} = FUN( MOD ),
    ?assertMatch( ok, RESULT ),
    ?assert( is_binary( BEAM ) ).


%% ===========================================================================
%% Internal Functionality
%% ===========================================================================

