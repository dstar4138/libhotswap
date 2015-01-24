%%%
%%% Unit Testing for libhotswap utility functions.
%%%
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

get_ast1_test_() ->
    {"Verify abstract syntax tree construction for modules.",
        ?SETUP({inparallel,
                [ fun validate_correctModule__get_ast1/0,
                  fun validate_badInput__get_ast1/0
                ]})
    }.

fun_to_ast1_test_() ->
    {"Verify abstract syntax tree construction for anonymous and named "
     "functions.",
        ?SETUP({inparallel,
                [ fun validate_anonFun__fun_to_ast1/0,
                  fun validate_externalFun__fun_to_ast1/0,
                  fun validate_outOfScope__fun_to_ast1/0
                ]})
    }. 

code_to_ast1_test_() ->
    {"Verify abstract syntax tree construction for Erlang Code as strings.",
        ?SETUP({inparallel,
                [ fun validate_terms__code_to_ast1/0,
                  fun validate_branches__code_to_ast1/0,
                  fun validate_anonFun__code_to_ast1/0, 
                  fun validate_externalFun__code_to_ast1/0
                ]})
    }. 

funcs1_test_() ->
    {"Verify AST function construction given a func (string/ast/fun/mfa).",
        ?SETUP({inparallel,
                [ fun validate_stringFuncs__funcs1/0,
                  fun validate_astFuncs__funcs1/0,
                  fun validate_funFuncs__funcs1/0,
                  fun validate_mfaFuncs__funcs1/0
                ]})
    }. 

ast_to_code1_test_() ->
    {"Verify AST to Source code conversion utility functionality.",
        ?SETUP({inparallel,
                [ fun validate_terms__ast_to_code1/0,
                  fun validate_branches__ast_to_code1/0,
                  fun validate_anonFun__ast_to_code1/0,
                  fun validate_externalFun__ast_to_code1/0
                ]})
    }. 

ast_by_mfa2_test_() ->
    {"Verify AST extraction from a module given an MFA and the module AST.",
        ?SETUP({inparallel,
                [ fun validate_simple__ast_by_mfa2/0,
                  fun validate_missing__ast_by_mfa2/0
                ]})
    }. 

ast_by_mfa1_test_() ->
    {"Verify AST extraction from a moduel given only the MFA.",
        ?SETUP({inparallel,
                [ fun validate_stdlibLookup__ast_by_mfa1/0,
                  fun validate_locallibLookup__ast_by_mfa1/0
                ]})
    }. 

inject_attributes2_test_() ->
    {"Verify AST attribute injection.",
        ?SETUP({inparallel,
                [ fun validate_correctSyntax__inject_attributes2/0 
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

%% TEST - Verify the AST returned is describing the module requested. 
validate_correctModule__get_ast1() ->
    FUN = fun libhotswap_util:get_ast/1,
    MODULES = [libhotswap, libhotswap_util, io, kernel, erl_syntax],
    CHECK = fun( MOD ) -> 
                    case FUN( MOD ) of
                        {ok, AST} -> lists:keyfind(module,3,AST);
                        ERROR -> ERROR
                    end        
            end,
    [?assertMatch({attribute,_,module,MOD}, CHECK(MOD)) || MOD <- MODULES].

%% TEST - Verify that any bad module input will result in an error. 
validate_badInput__get_ast1() ->
    FUN = fun libhotswap_util:get_ast/1,
    ?assertMatch( error, FUN( non_existant_module ) ).

%% TEST - Verify anonymous functions are converted correctly into ASTs.
validate_anonFun__fun_to_ast1() ->
    FUN = fun libhotswap_util:fun_to_ast/1,
    ANON = fun() -> ok end,
    ANON_AST = {'fun',1,{clauses,[{clause,1,[],[],[{atom,1,'ok'}]}]}},
    ?assertMatch( {ok, ANON_AST}, FUN( ANON ) ). 

%% TEST - Verify external (fun m:f/a) referenced functions convert correctly.
validate_externalFun__fun_to_ast1() ->
    FUN = fun libhotswap_util:fun_to_ast/1,


validate_outOfScope__fun_to_ast1()->ok.
validate_terms__code_to_ast1()->ok.
validate_branches__code_to_ast1()->ok.
validate_anonFun__code_to_ast1()->ok.
validate_externalFun__code_to_ast1()->ok.
validate_stringFuncs__funcs1()->ok.
validate_astFuncs__funcs1()->ok.
validate_funFuncs__funcs1()->ok.
validate_mfaFuncs__funcs1()->ok.
validate_terms__ast_to_code1()->ok.
validate_branches__ast_to_code1()->ok.
validate_anonFun__ast_to_code1()->ok.
validate_externalFun__ast_to_code1()->ok.
validate_simple__ast_by_mfa2()->ok.
validate_missing__ast_by_mfa2()->ok.
validate_stdlibLookup__ast_by_mfa1()->ok.
validate_locallibLookup__ast_by_mfa1()->ok.
validate_correctSyntax__inject_attributes2()->ok.


%% ===========================================================================
%% Internal Functionality
%% ===========================================================================

