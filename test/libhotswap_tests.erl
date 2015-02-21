%%%
%%% Unit Testing for LibHotSwap API.
%%%
-module( libhotswap_tests ).
-include_lib("eunit/include/eunit.hrl").
-define(SETUP( Fs ), {setup, fun() -> [] end, fun(_) -> [] end, Fs}). 
-define(SETUP_WITH_SERVER( Fs ), {setup, fun start_server/0, fun stop_server/1, Fs}).

%% ===========================================================================
%% Test Descriptions
%% ===========================================================================

check_vsn1_test_() ->
    {"Test getting the vsn number of a module.",
        ?SETUP({inparallel, 
                [ fun validate_dummy__vsn1/0
                ]})
    }.

check_exports1_test_() ->
    {"Test getting the exports of a module as MFAs.",
        ?SETUP({inparallel, 
                [ fun validate_dummy__exports1/0
                ]})
    }.

check_get_code1_test_() ->
    {"Test source code extraction from arbitrary functions/terms/modules.",
        ?SETUP({inparallel,
                [ fun validate_module__get_code1/0,
                  fun validate_funcs__get_code1/0,
                  fun validate_term__get_code1/0
                ]})
    }.

check_get_ast1_test_() ->
    {"Test AST extraction from arbitrary functions/terms/modules.",
        ?SETUP({inparallel,
                [ fun validate_module__get_ast1/0,
                  fun validate_funcs__get_ast1/0,
                  fun validate_term__get_ast1/0
                ]})
    }.

check_happy_check_server0_test_() ->
    {"Test that the server starts up an shuts down successfully.",
        ?SETUP_WITH_SERVER(fun validate_positive__check_server0/1)
    }.

check_unhappy_check_server0_test_() ->
    {"Test that the server apis works when server is down.",
        ?SETUP({inorder,
                [ fun validate_negative__check_server0/0
                ]})
    }.

check_add_export2_test_() ->
    {"Testing export injection works.",
        [?SETUP({inorder,[ fun validate_dummy__add_export2/0,
                           fun validate_dummyMulti__add_export2/0 ]} ),
         ?SETUP_WITH_SERVER(fun validate_withServer__add_export2/1) 
        ]
    }.

check_remove_export1_test_() ->
    {"Testing removal of exports.",
        [?SETUP({inorder,[ fun validate_dummy__remove_export1/0,
                           fun validate_dummyMulti__remove_export1/0 ]} ),
         ?SETUP_WITH_SERVER(fun validate_withServer__remove_export1/1) 
        ]
    }.

check_rewrite2_test_() ->
    {"Test brute force rewrite of exported functions in modules.",
        [?SETUP({inorder,[ fun validate_dummy__rewrite2/0,
                           fun validate_dummyMulti__rewrite2/0 ]} ),
         ?SETUP_WITH_SERVER(fun validate_withServer__rewrite2/1) 
        ]
    }.

check_inject_in_function3_test_() ->
    {"Test arbitrary code injection, into function clauses.",
        [?SETUP({inorder,[ fun validate_dummy__inject_in_function3/0,
                           fun validate_dummyMulti__inject_in_function3/0 ]} ),
         ?SETUP_WITH_SERVER(fun validate_withServer__inject_in_function3/1) 
        ]
    }.

check_add_new_clause3_test_() -> 
    {"Test function clause injection.",
        [?SETUP({inorder,[ fun validate_dummy__add_new_clause3/0,
                           fun validate_dummyMulti__add_new_clause3/0 ]} ),
         ?SETUP_WITH_SERVER(fun validate_withServer__add_new_clause3/1) 
        ]
    }.

check_rollback2_test_() ->
    {"Test code server rollback functionality.",
         ?SETUP_WITH_SERVER(fun validate_dummy__rollback2/1) 
    }.

%% ===========================================================================
%% Actual Tests
%% ===========================================================================

%% TEST - The vsn of the dummy module is set via the vsn attribute in the source
%%        and should always be 42.
validate_dummy__vsn1() ->
    FUN = fun libhotswap:vsn/1,
    MOD = libhotswap_dummy,
    ?assertMatch( {ok, 42}, FUN( MOD ) ).

%% TEST - The exports of the dummy module are consistent as long as the Erlang
%%        compiler injected functions dont change signature.
validate_dummy__exports1() ->
    FUN = fun libhotswap:exports/1,
    MOD = libhotswap_dummy,
    EXP = [{MOD,test,0},{MOD,test,1},{MOD,module_info,0},{MOD,module_info,1}],
    ?assertMatch( {ok, EXP}, FUN( MOD ) ).

validate_module__get_code1()->ok.
validate_funcs__get_code1()->ok.
validate_term__get_code1()->ok.
validate_module__get_ast1()->ok.
validate_funcs__get_ast1()->ok.
validate_term__get_ast1()->ok.

%% TEST - Make sure we can get at the server if it's running. 
validate_positive__check_server0( Pid ) ->
    FUN = fun libhotswap:check_server/0,
    [ ?_assertMatch( Pid, whereis( libhotswap_server )),
      ?_assertMatch( {ok, Pid}, FUN() ) ].

%% TEST - This checks to makes sure we correctly return a false when the 
%%        server is not running.
validate_negative__check_server0() ->
    FUN = fun libhotswap:check_server/0,
    ?assertMatch( undefined, whereis(libhotswap_server) ),
    ?assertMatch( false, FUN() ). 

%% TEST - Add another export to a module without the libhotswap server wrapper
%%        in place.
validate_dummy__add_export2() ->
    FUN = fun libhotswap:add_export/2,
    MOD = libhotswap_dummy,
    MFA = {MOD, alt, 0}, % Essentially add test/0 again but with another name
    TST = fun MOD:test/0,
    AFTER = [{MOD,alt,0},
             {MOD,test,0},{MOD,test,1},{MOD,module_info,0},{MOD,module_info,1}],
    ?assertMatch( {ok, _}, FUN( MFA, TST ) ),
    ?assertMatch( {ok, AFTER}, libhotswap:exports( MOD ) ),
    ?assertMatch( ok, MOD:alt() ).

%% TEST - Validate that without the server, we cannot keep more than one 
%%        injection at a time.
validate_dummyMulti__add_export2() ->
    FUN = fun libhotswap:add_export/2,
    MOD = libhotswap_dummy,
    MFA1 = {MOD, alt1, 0}, 
    MFA2 = {MOD, alt2, 0},
    TST = fun MOD:test/0,
    AFTER = [{MOD,alt2,0}, % Only the second should show up.
             {MOD,test,0},{MOD,test,1},{MOD,module_info,0},{MOD,module_info,1}],
    ?assertMatch( {ok, _}, FUN( MFA1, TST ) ),
    ?assertMatch( {ok, _}, FUN( MFA2, TST ) ),
    ?assertMatch( {ok, AFTER}, libhotswap:exports( MOD ) ),
    ?assertMatch( ok, MOD:alt2() ).

%% TEST - Once the server has been set up, we can now make sure both get added.
validate_withServer__add_export2( _Pid ) ->
    FUN = fun libhotswap:add_export/2,
    MOD = libhotswap_dummy,
    MFA1 = {MOD, alt1, 0},
    MFA2 = {MOD, alt2, 0},
    TST = fun MOD:test/0,
    BEFORE = [{MOD,test,0},{MOD,test,1},{MOD,module_info,0},{MOD,module_info,1}],
    AFTER1 = [MFA1|BEFORE], 
    AFTER2 = [MFA1,MFA2|BEFORE],
    [ ?_assertMatch( {ok, _}, FUN( MFA1, TST ) ),
      ?_assertMatch( {ok, AFTER1}, libhotswap:exports( MOD ) ),
      ?_assertMatch( ok, MOD:alt1() ),
      ?_assertMatch( {ok, _}, FUN( MFA2, TST ) ),
      ?_assertMatch( {ok, AFTER2}, libhotswap:exports( MOD ) ),
      ?_assertMatch( ok, MOD:alt2() ) ].

validate_dummy__remove_export1()->ok.
validate_dummyMulti__remove_export1()->ok.
validate_withServer__remove_export1(_Pid)->[].
validate_dummy__rewrite2()->ok.
validate_dummyMulti__rewrite2()->ok.
validate_withServer__rewrite2(_Pid)->[].
validate_dummy__inject_in_function3()->ok.
validate_dummyMulti__inject_in_function3()->ok.
validate_withServer__inject_in_function3(_Pid)->[].
validate_dummy__add_new_clause3()->ok.
validate_dummyMulti__add_new_clause3()->ok.
validate_withServer__add_new_clause3(_Pid)->[].
validate_dummy__rollback2(_Pid)->[].
 
%% ===========================================================================
%% Server Setup
%% ===========================================================================

% The server should start correctly at the beginning of each test. The only 
% time when this function should return an error is when the server is already
% up.
start_server() -> 
    {ok,Pid} = libhotswap_server:start_link(),
    Pid.

% The server should also stop correctly at the ending of each test. We ignore
% the application heirarchy and assume OTP can handle itself.
stop_server(_) -> 
    ok = libhotswap_server:stop().

