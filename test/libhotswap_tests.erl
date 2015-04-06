%%%
%%% Unit Testing for LibHotSwap API.
%%%
-module( libhotswap_tests ).
-include_lib("eunit/include/eunit.hrl").
-define(SETUP( Fs ), {setup, fun() -> [] end, fun(_) -> [] end, Fs}).
-define(SETUP_WITH_SERVER( Fs ), {setup, fun start_server/0, fun stop_server/1, Fs}).

% Override cache directory location for testing.
-define(TESTING_CACHE_DIR,"./.eunit/fake_cache").

% Dummy module MFA description
-define(DUMMY_MFAS,[{libhotswap_dummy,test,0},{libhotswap_dummy,test,1},
                    {libhotswap_dummy,alt,0},{libhotswap_dummy,alt,1},
                    {libhotswap_dummy,module_info,0},
                    {libhotswap_dummy,module_info,1}]).

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
    ?assertMatch( {ok, ?DUMMY_MFAS}, FUN( MOD ) ).

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
    NEW = added,
    MFA = {MOD, NEW, 0}, % Essentially add test/0 again but with another name
    TST = fun MOD:test/0,
    AFTER = [MFA | ?DUMMY_MFAS],
    ?assertMatch( {ok, _}, FUN( MFA, TST ) ),
    ?assertMatch( {ok, AFTER}, libhotswap:exports( MOD ) ),
    ?assertMatch( ok, MOD:NEW() ),
    reset( MOD ).

%% TEST - Validate that without the server, we cannot keep more than one
%%        injection at a time.
validate_dummyMulti__add_export2() ->
    FUN = fun libhotswap:add_export/2,
    MOD = libhotswap_dummy,
    [NEW0,NEW1] = [added0, added1],
    [MFA0,MFA1] = [{MOD, NEW0, 0},{MOD, NEW1, 0}],
    TST = fun MOD:test/0,
    AFTER = [MFA1 | ?DUMMY_MFAS], % Only the second should show up.
    ?assertMatch( {ok, _}, FUN( MFA0, TST ) ),
    ?assertMatch( {ok, _}, FUN( MFA1, TST ) ),
    ?assertMatch( {ok, AFTER}, libhotswap:exports( MOD ) ),
    ?assertMatch( ok, MOD:NEW1() ),
    reset( MOD ).

%% TEST - Once the server has been set up, we can now make sure both get added.
validate_withServer__add_export2( _Pid ) ->
    FUN = fun libhotswap:add_export/2,
    MOD = libhotswap_dummy,
    MFA1 = {MOD, alt1, 0},
    MFA2 = {MOD, alt2, 0},
    TST = fun MOD:test/0,
    AFTER1 = [MFA1|?DUMMY_MFAS],
    AFTER2 = [MFA1,MFA2|?DUMMY_MFAS],
    [ ?_assertMatch( {ok, _}, FUN( MFA1, TST ) ),
      ?_assertMatch( {ok, AFTER1}, libhotswap:exports( MOD ) ),
      ?_assertMatch( ok, MOD:alt1() ),
      ?_assertMatch( {ok, _}, FUN( MFA2, TST ) ),
      ?_assertMatch( {ok, AFTER2}, libhotswap:exports( MOD ) ),
      ?_assertMatch( ok, MOD:alt2() ) ].

%% TEST - Simple test of an export removal.
validate_dummy__remove_export1() ->
    FUN = fun libhotswap:remove_export/1,
    MOD = libhotswap_dummy,
    [MFA | AFTER] = ?DUMMY_MFAS,
    ?assertMatch( {ok, ?DUMMY_MFAS}, libhotswap:exports( MOD ) ),
    ?assertMatch( {ok, _}, FUN( MFA ) ),
    ?assertMatch( {ok, AFTER}, libhotswap:exports( MOD ) ),
    reset( MOD ).

%% TEST - Attempt two function removals, without the server started. Notice
%%        only the last one sticks.
validate_dummyMulti__remove_export1() ->
    FUN = fun libhotswap:remove_export/1,
    MOD = libhotswap_dummy,
    [MFA1 | AFTER1] = ?DUMMY_MFAS,
    [MFA2 | TMP] = AFTER1,
    AFTER2 = [MFA1|TMP],
    ?assertMatch( {ok, ?DUMMY_MFAS}, libhotswap:exports( MOD ) ),
    ?assertMatch( {ok, _}, FUN( MFA1 ) ),
    ?assertMatch( {ok, AFTER1}, libhotswap:exports( MOD ) ),
    ?assertMatch( {ok, _}, FUN( MFA2 ) ),
    ?assertMatch( {ok, AFTER2}, libhotswap:exports( MOD ) ),
    reset( MOD ).

%% TEST - Attempt the previous test, but this time with the server started,
%%        note that this time it works.
validate_withServer__remove_export1(_Pid)->
    FUN = fun libhotswap:remove_export/1,
    MOD = libhotswap_dummy,
    [MFA1 | AFTER1] = ?DUMMY_MFAS,
    [MFA2 | AFTER2] = AFTER1,
    [ ?_assertMatch( {ok, ?DUMMY_MFAS}, libhotswap:exports( MOD ) ),
      ?_assertMatch( {ok, _}, FUN( MFA1 ) ),
      ?_assertMatch( {ok, AFTER1}, libhotswap:exports( MOD ) ),
      ?_assertMatch( {ok, _}, FUN( MFA2 ) ),
      ?_assertMatch( {ok, AFTER2}, libhotswap:exports( MOD ) ) ].

%% TEST - Brute force function rewrite. This will override all clauses.
validate_dummy__rewrite2() ->
    FUN = fun libhotswap:rewrite/2,
    MOD = libhotswap_dummy,
    MFA = {MOD, test, 0 },
    TST = fun math:pi/0,
    RES = math:pi(),
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( {ok,_}, FUN( MFA, TST ) ),
    ?assertMatch( RES, MOD:test() ),
    reset( MOD ).

%% TEST - Attempt brute force function rewrite twice without the code server,
%%        showing only the last rewrite sticks.
validate_dummyMulti__rewrite2() ->
    FUN = fun libhotswap:rewrite/2,
    MOD = libhotswap_dummy,
    MFA0 = {MOD,test,0},
    MFA1 = {MOD,test,1},
    {TST0,RES0} = {fun MOD:alt/0, MOD:alt()},
    {TST1,RES1} = {fun MOD:alt/1, MOD:alt(0)},
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( {ok,_}, FUN( MFA0, TST0 ) ),
    ?assertMatch( RES0, MOD:test() ),
    ?assertMatch( {ok,_}, FUN( MFA1, TST1 ) ),
    ?assertMatch( RES1, MOD:test(0) ),
    ?assertMatch( ok, MOD:test() ),
    reset( MOD ).

%% TEST - Attempt multiple brute force rewrites, with the code server running.
validate_withServer__rewrite2(_Pid) ->
    FUN = fun libhotswap:rewrite/2,
    MOD = libhotswap_dummy,
    MFA0 = {MOD,test,0},
    MFA1 = {MOD,test,1},
    {TST0,RES0} = {fun MOD:alt/0, MOD:alt()},
    {TST1,RES1} = {fun MOD:alt/1, MOD:alt(0)},
    [ ?_assertMatch( ok, MOD:test() ),
      ?_assertMatch( {ok,_}, FUN( MFA0, TST0 ) ),
      ?_assertMatch( RES0, MOD:test() ),
      ?_assertMatch( {ok,_}, FUN( MFA1, TST1 ) ),
      ?_assertMatch( RES1, MOD:test(0) ),
      ?_assertMatch( RES0, MOD:test() )].

%% TEST - Inject functionality into a module's export and test that this
%%        new functionality executes when the function runs.
validate_dummy__inject_in_function3() ->
    FUN = fun libhotswap:inject_in_function/3,
    MOD = libhotswap_dummy,
    LOCATION1 = {0,[0]}, % 0'th line (top) of the [0'th] clause(s).
    INJECTED = fun libhotswap_util:injected/0,
    libhotswap_util:prime_injection(),
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( 0, libhotswap_util:test_injection() ),
    ?assertMatch( {ok,_}, FUN( {MOD,test,0}, INJECTED, LOCATION1 ) ),
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( 1, libhotswap_util:test_injection() ),
    reset( MOD ).

%% TEST - Attempt multiple injections, without the code server to make sure
%%        only the latter is picked up.
validate_dummyMulti__inject_in_function3() ->
    FUN = fun libhotswap:inject_in_function/3,
    MOD = libhotswap_dummy,
    LOCATION1 = {0,[0]},
    INJECTED = fun libhotswap_util:injected/0,
    libhotswap_util:prime_injection(),
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( 0, libhotswap_util:test_injection() ),
    ?assertMatch( {ok,_}, FUN( {MOD,test,0}, INJECTED, LOCATION1 ) ),
    ?assertMatch( {ok,_}, FUN( {MOD,alt,0}, INJECTED, LOCATION1 ) ),
    ?assertMatch( ok, MOD:test() ),
    ?assertMatch( 0, libhotswap_util:test_injection() ),
    ?assertMatch( okay, MOD:alt() ),
    ?assertMatch( 1, libhotswap_util:test_injection() ),
    reset( MOD ).

%% TEST - Inject multiple functions with code, and make sure they both run as
%%        expected as long as the code server is running.
validate_withServer__inject_in_function3(_Pid) ->
    FUN = fun libhotswap:inject_in_function/3,
    MOD = libhotswap_dummy,
    LOCATION1 = {0,[0]},
    INJECTED = fun libhotswap_util:injected/0,
    [ ?_assertMatch( ok, libhotswap_util:prime_injection() ),
      ?_assertMatch( ok, MOD:test() ),
      ?_assertMatch( 0, libhotswap_util:test_injection() ),
      ?_assertMatch( {ok,_}, FUN( {MOD,test,0}, INJECTED, LOCATION1 ) ),
      ?_assertMatch( {ok,_}, FUN( {MOD,alt,0}, INJECTED, LOCATION1 ) ),
      ?_assertMatch( ok, MOD:test() ),
      ?_assertMatch( 1, libhotswap_util:test_injection() ),
      ?_assertMatch( okay, MOD:alt() ),
      ?_assertMatch( 2, libhotswap_util:test_injection() ) ].

%% TEST - Inject a new clause into an exported function.
validate_dummy__add_new_clause3() ->
    FUN = fun libhotswap:add_new_clause/3,
    MOD = libhotswap_dummy,
    LOCATION = 0, % Order the clause at the very beginning.
    CLAUSE = fun libhotswap_util:injected_clause1/1,
    ?assertMatch( {ok,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
    ?assertMatch( {ok,_}, FUN( {MOD,test,1}, CLAUSE, LOCATION ) ),
    ?assertMatch( {24,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
    reset( MOD ).

%% TEST - Attempt multiple clause injections without the code server running.
%%        Only the latter should stick.
validate_dummyMulti__add_new_clause3() ->
    FUN = fun libhotswap:add_new_clause/3,
    MOD = libhotswap_dummy,
    LOCATION = 0, % Order the clause at the very beginning.
    CLAUSE1 = fun libhotswap_util:injected_clause1/1,
    CLAUSE2 = fun libhotswap_util:injected_clause2/1,
    ?assertMatch( {ok,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
    ?assertMatch( {ok,_}, FUN( {MOD,test,1}, CLAUSE1, LOCATION ) ),
    ?assertMatch( {ok,_}, FUN( {MOD,test,1}, CLAUSE2, LOCATION ) ),
    ?assertMatch( {ok,42,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
    reset( MOD ).

%% TEST - Perform multiple clause injections with the code server running.
validate_withServer__add_new_clause3(_Pid) ->
    FUN = fun libhotswap:add_new_clause/3,
    MOD = libhotswap_dummy,
    LOCATION = 0, % Order the clause at the very beginning.
    CLAUSE1 = fun libhotswap_util:injected_clause1/1,
    CLAUSE2 = fun libhotswap_util:injected_clause2/1,
    [ ?_assertMatch( {ok,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
      ?_assertMatch( {ok,_}, FUN( {MOD,test,1}, CLAUSE1, LOCATION ) ),
      ?_assertMatch( {24,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
      ?_assertMatch( {ok,_}, FUN( {MOD,test,1}, CLAUSE2, LOCATION ) ),
      ?_assertMatch( {24,42,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ) ].

%% TEST - Make sure rollbacks work as expected when the code server is running.
validate_dummy__rollback2(_Pid) ->
    FUN1 = fun libhotswap:add_new_clause/3,
    FUN2 = fun libhotswap:rollback/2,
    MOD = libhotswap_dummy,
    LOCATION = 0, % Order the clause at the very beginning.
    CLAUSE1 = fun libhotswap_util:injected_clause1/1,
    CLAUSE2 = fun libhotswap_util:injected_clause2/1,
    [ ?_assertMatch( {ok,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
      ?_assertMatch( {ok,_}, FUN1( {MOD,test,1}, CLAUSE1, LOCATION ) ),
      ?_assertMatch( {ok,_}, FUN1( {MOD,test,1}, CLAUSE2, LOCATION ) ),
      ?_assertMatch( {24,42,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
      ?_assertMatch( ok, FUN2( MOD, 1 ) ),
      ?_assertMatch( {24,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} ),
      ?_assertMatch( {ok,_}, FUN1( {MOD,test,1}, CLAUSE2, LOCATION ) ),
      ?_assertMatch( ok, FUN2( MOD, 2 ) ),
      ?_assertMatch( {ok,ok,ok}, {MOD:test(42),MOD:test(24),MOD:test(0)} )
    ].

%% ===========================================================================
%% Server Setup
%% ===========================================================================

% The server should start correctly at the beginning of each test. The only
% time when this function should return an error is when the server is already
% up.
start_server() ->
    application:set_env(libhotswap, cache_dir, ?TESTING_CACHE_DIR),
    {ok,Pid} = libhotswap_server:start_link(),
    Pid.

% The server should also stop correctly at the ending of each test. We ignore
% the application heirarchy and assume OTP can handle itself.
stop_server(_) ->
    ok = libhotswap_server:purge(),
    ok = libhotswap_server:stop().

% Essentially the same as c:l/1. Will purge whatever the code store has and
% reset it with the module from the Path.
reset( Module ) ->
    code:purge( Module ),
    code:load_file( Module ).
