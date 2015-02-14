%%%
%%% Unit Testing for libhotswap API.
%%%
-module( libhotswap_tests ).
-include_lib("eunit/include/eunit.hrl").
-define(SETUP( Fs ), {setup, fun() -> [] end, fun(_) -> [] end, Fs}). 

%% ===========================================================================
%% Test Descriptions
%% ===========================================================================

check_vsn1_test_() ->
    {"Test getting the vsn number of a module.",
        ?SETUP({inparallel, 
                [ fun validate_dummy__vsn1/0
                ]})
    }.

check_version1_test_() ->
    {"Test getting the version number of a module.",
        ?SETUP({inparallel, 
                [ fun validate_dummy__version1/0
                ]})
    }.

check_exports1_test_() ->
    {"Test getting the exports of a module as MFAs.",
        ?SETUP({inparallel, 
                [ fun validate_dummy__exports1/0
                ]})
    }.



%% ===========================================================================
%% Actual Tests
%% ===========================================================================

validate_dummy__vsn1() ->
    FUN = fun libhotswap:vsn/1,
    MOD = libhotswap_dummy,
    ?assertMatch( {ok, 42}, FUN( MOD ) ).


validate_dummy__version1() ->
    FUN = fun libhotswap:version/1,
    MOD = libhotswap_dummy,
    ?assertMatch( {ok, "5.0.3"}, FUN(MOD) ).

validate_dummy__exports1() ->
    FUN = fun libhotswap:exports/1,
    MOD = libhotswap_dummy,
    EXP = [{MOD,test,0},{MOD,test,1},{MOD,module_info,0},{MOD,module_info,1}],
    ?assertMatch( {ok, EXP}, FUN( MOD ) ).


