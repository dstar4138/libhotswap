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

%% These fail due to EUnit's parse_transformation of function clauses.
%fun_to_ast1_test_() ->
%    {"Verify abstract syntax tree construction for anonymous and named "
%     "functions.",
%        ?SETUP({inparallel,
%                [ fun validate_anonFun__fun_to_ast1/0,
%                  fun validate_externalFun__fun_to_ast1/0,
%                  fun validate_outOfScope__fun_to_ast1/0,
%                  fun validate_namedFunction__fun_to_ast1/0
%                ]})
%    }.

code_to_ast1_test_() ->
    {"Verify abstract syntax tree construction for Erlang Code as strings.",
        ?SETUP({inparallel,
                [ fun validate_terms__code_to_ast1/0,
                  fun validate_branches__code_to_ast1/0,
                  fun validate_anonFun__code_to_ast1/0,
                  fun validate_externalFun__code_to_ast1/0
                ]})
    }.

%% These fail due to EUnit's pares_transofmation of function clauses.
funcs1_test_() ->
    {"Verify AST function construction given a func (string/ast/fun/mfa).",
        ?SETUP({inparallel,
                [ fun validate_stringFuncs__funcs1/0,
                  fun validate_astFuncs__funcs1/0,
%                  fun validate_funFuncs__funcs1/0,
%                  fun validate_mfaFuncs__funcs1/0,
                  fun validate_multiFuncs__funcs1/0,
                  fun validate_nonFuncs__funcs1/0
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
    {"Verify AST extraction from a module given only the MFA.",
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
    ?assertEqual( {error,unforced}, FUN( MODULE, false ) ),
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
%% Note: this doesn't work during Eunit, due to a renaming that happens. So
%% I've disabled it.
%validate_anonFun__fun_to_ast1() ->
%    FUN = fun libhotswap_util:fun_to_ast/1,
%    ANON_AST = {'fun',1,{clauses,[{clause,1,[],[],[{atom,1,'ok'}]}]}},
%    ?assertMatch( {ok, ANON_AST}, FUN( fun() -> ok end ) ).

%% TEST - Verify external (fun m:f/a) referenced functions convert correctly.
%validate_externalFun__fun_to_ast1() ->
%    FUN = fun libhotswap_util:fun_to_ast/1,
%    DUMMYFUN = fun libhotswap_dummy:test/0,
%    DUMMYAST = {function,4,test,0,[{clause,4,[],[],[{atom,4,ok}]}]},
%    ?assertMatch( {ok, DUMMYAST}, FUN( DUMMYFUN ) ).

%% TEST - Currently a function which has values defined out of scope,
%%        should throw an error. Once our environment replacement mechanism
%%        is in place, it should work fine.
%validate_outOfScope__fun_to_ast1() ->
%    FUN = fun libhotswap_util:fun_to_ast/1,
%    Var = test,
%    DummyFun = fun() -> Var end,
%    ?assertMatch( {error, outofscope}, FUN( DummyFun ) ).

%% TEST - We also currently can't handle named functions as they can have
%%        unintended side effects and we don't quite know how we want their
%%        injection to work in the long run.
%validate_namedFunction__fun_to_ast1() ->
%    FUN = fun libhotswap_util:fun_to_ast/1,
%    DummyFun = fun _DummyFun() -> ok end,
%    ?assertMatch( {error, named}, FUN( DummyFun ) ).


%% TEST - Turn standard terms into abstract syntax trees.
validate_terms__code_to_ast1() ->
    FUN = fun libhotswap_util:code_to_ast/1,
    ?assertMatch( {ok, [{var,1,'_'}]}, FUN( "_." ) ),
    ?assertMatch( {ok, [{atom,1,ok}]}, FUN( "ok." ) ),
    ?assertMatch( {ok, [{integer,1,1}]}, FUN( "1.") ),
    ?assertMatch( {ok, [{float,1,1.0}]}, FUN( "1.0.") ),
    ?assertMatch( {ok, [{integer,1,22}]}, FUN( "16#16.") ),
    ?assertMatch( {ok, [{string,1,"hi"}]}, FUN( "\"hi\".") ),
    ?assertMatch( {ok, [{tuple,1,[]}]}, FUN( "{}.") ),
    ?assertMatch( {ok, [{tuple,1,[{integer,1,1}]}]}, FUN( "{1}.") ),
    ?assertMatch( {ok, [{nil,1}]}, FUN( "[].") ),
    ?assertMatch( {ok, [{cons,1,{integer,1,1},{nil,1}}]}, FUN( "[1].") ),
    ?assertMatch( {ok, [{bin,1,[]}]}, FUN( "<<>>.") ),
    ?assertMatch( {ok, [{bin,1,[{bin_element,1,{integer,1,1},default,default}]}]}, FUN( "<<1>>.") ).

%% TEST - Turn branching statements into their abstract syntax trees.
validate_branches__code_to_ast1() ->
    FUN = fun libhotswap_util:code_to_ast/1,
    ?assertMatch({ok,[{'case',1,{atom,1,ok},
                                [{clause,1,[{var,1,'_'}],
                                [[{call,1,{atom,1,is_atom},[{atom,1,ok}]}]],
                                [{atom,1,ok}]}]}]},
                  FUN( "case ok of _ when is_atom(ok) -> ok end." ) ),
    ?assertMatch( {ok,[{'if',1,[{clause,1,[],[[{atom,1,true}]],[{atom,1,ok}]}]}]},
                  FUN( "if true -> ok end." ) ).

%% TEST - Turn Anonymous into ASTs.
validate_anonFun__code_to_ast1() ->
    FUN = fun libhotswap_util:code_to_ast/1,
    ?assertMatch( {ok,[{'fun',1,{clauses,[{clause,1,[],[],[{atom,1,ok}]}]}}]},
                  FUN( "fun() -> ok end." ) ),
    ?assertMatch( {ok, [{'fun',1,{clauses,[{clause,1,[{var,1,'X'}],[],[{var,1,'X'}]}]}}]},
                  FUN( "fun(X) -> X end." ) ).

%% TEST - Turn external function references into ASTs.
validate_externalFun__code_to_ast1() ->
    FUN = fun libhotswap_util:code_to_ast/1,
    ?assertMatch( {ok,[{'fun',1,{function,{atom,1,libhotswap_dummy},
                                          {atom,1,test},
                                          {integer,1,0}}}]},
                  FUN( "fun libhotswap_dummy:test/0." ) ).

%% TEST - Check that a string function conversion works.
validate_stringFuncs__funcs1() ->
    FUN = fun libhotswap_util:funcs/1,
    ?assertMatch( {ok,{'fun',1,{function,{atom,1,libhotswap_dummy},
                                          {atom,1,test},
                                          {integer,1,0}}}},
                  FUN( "fun libhotswap_dummy:test/0." ) ),
    ?assertMatch( {ok,{'fun',1,{clauses,[{clause,1,[],[],[{atom,1,ok}]}]}}},
                  FUN( "fun() -> ok end." ) ).

%% TEST - Absract Syntax Trees to their function values. Namely, make sure valid
%%        ASTs are validated as funcs.
validate_astFuncs__funcs1() ->
    FUN = fun libhotswap_util:funcs/1,
    Ext = {'fun',1,{function,{atom,1,libhotswap_dummy},
                                          {atom,1,test},
                                          {integer,1,0}}},
    Anon = {'fun',1,{clauses,[{clause,1,[],[],[{atom,1,ok}]}]}},
    ?assertMatch( {ok, Ext}, FUN( Ext ) ),
    ?assertMatch( {ok, Anon}, FUN( Anon ) ),
    % Make sure expressions get stripped.
    ?assertMatch( {ok, Ext}, FUN( [Ext] ) ),
    ?assertMatch( {ok, Anon}, FUN( [Anon] ) ),
    ?assertMatch( {error,badarg}, FUN( [{integer,1,1}] ) ).

%% TEST - Validate that Erlang functions and funs are converted into their func
%%        (or... really, ast) counterparts.
%validate_funFuncs__funcs1() ->
%    FUN = fun libhotswap_util:fun_to_ast/1,
%    ANON = fun() -> ok end,
%    ANON_AST = {'fun',1,{clauses,[{clause,1,[],[],[{atom,1,'ok'}]}]}},
%    ?assertMatch( {ok, ANON_AST}, FUN( ANON ) ).

%% TEST - Validate that a 'fun M:F/A' form is converted to a func correctly.
%validate_mfaFuncs__funcs1() ->
%    FUN = fun libhotswap_util:funcs/1,
%    DUMMYFUN = fun libhotswap_dummy:test/0,
%    DUMMYAST = {function,4,test,0,[{clause,4,[],[],[{atom,4,ok}]}]},
%    ?assertMatch( {ok, DUMMYAST}, FUN( DUMMYFUN ) ).

%% TEST - If there are multiple expressions in the string, this should cause a
%%        error (it can't guess which one you meant).
validate_multiFuncs__funcs1() ->
    FUN = fun libhotswap_util:funcs/1,
    ?assertMatch( {error, toomanyexpr}, FUN( "ok,ok." ) ).

%% TEST - Non functions, make sure they are actually returning errors and not
%%        faking it.
validate_nonFuncs__funcs1() ->
    FUN = fun libhotswap_util:funcs/1,
    ?assertMatch( {error,badarg}, FUN( "ok." ) ),
    ?assertMatch( {error,badarg}, FUN( ok ) ),
    ?assertMatch( {error,badarg}, FUN( {} ) ),
    ?assertMatch( {error,badarg}, FUN( 1.0 ) ),
    ?assertMatch( {error,badarg}, FUN( 1 ) ),
    ?assertMatch( {error,{0,erl_parse,_}}, FUN( [] ) ).

%% TEST - Check to make sure all simple terms convert back into source code.
validate_terms__ast_to_code1() ->
    FUN = fun libhotswap_util:ast_to_code/1,
    ?assertMatch( {ok, "_"}, FUN( [{var,1,'_'}] ) ),
    ?assertMatch( {ok, "_"}, FUN( {var,1,'_'} ) ), % Also check without sequencing
    ?assertMatch( {ok, "ok"}, FUN( [{atom,1,ok}] ) ),
    ?assertMatch( {ok, "1"}, FUN( [{integer,1,1}] ) ),
    ?assertMatch( {ok, "1.0"}, FUN( [{float,1,1.0}] ) ),
    ?assertMatch( {ok, "\"hi\""}, FUN([{string,1,"hi"}])),
    ?assertMatch( {ok, "{}"}, FUN( [{tuple,1,[]}] ) ),
    ?assertMatch( {ok, "{1}"}, FUN( [{tuple,1,[{integer,1,1}]}] ) ),
    ?assertMatch( {ok, "[]"}, FUN( [{nil,1}] ) ),
    ?assertMatch( {ok, "[1]"}, FUN( [{cons,1,{integer,1,1},{nil,1}}] ) ),
    ?assertMatch( {ok, "<<>>"}, FUN( [{bin,1,[]}] ) ),
    ?assertMatch( {ok, "<<1>>"}, FUN( [{bin,1,[{bin_element,1,{integer,1,1},default,default}]}] ) ).

%% TEST - Check to make sure the branches get converted back correctly.
validate_branches__ast_to_code1() ->
    FUN = fun libhotswap_util:ast_to_code/1,
    ?assertMatch({ok,"case ok of _ when is_atom(ok) -> ok end"},
                 FUN( [{'case',1,{atom,1,ok},
                                [{clause,1,[{var,1,'_'}],
                                [[{call,1,{atom,1,is_atom},[{atom,1,ok}]}]],
                                [{atom,1,ok}]}]}] ) ),
    ?assertMatch( {ok,"if true -> ok end"},
                  FUN( [{'if',1,[{clause,1,[],[[{atom,1,true}]],
                                               [{atom,1,ok}]}]}] ) ).

%% TEST - Check to make sure the anonymous functions convert back to code
%%      corretly.
validate_anonFun__ast_to_code1() ->
    FUN = fun libhotswap_util:ast_to_code/1,
    ?assertMatch( {ok,"fun () -> ok end"},
                  FUN( [{'fun',1,{clauses,[{clause,1,[],[],[{atom,1,ok}]}]}}] ) ),
    ?assertMatch( {ok, "fun (X) -> X end"},
                  FUN( [{'fun',1,{clauses,[{clause,1,[{var,1,'X'}],
                                          [],
                                          [{var,1,'X'}]}]}}] ) ).

%% TEST - Check that external function references convert to code correctly.
validate_externalFun__ast_to_code1() ->
    FUN = fun libhotswap_util:ast_to_code/1,
    ?assertMatch( {ok,"fun libhotswap_dummy:test/0"},
                  FUN( [{'fun',1,{function,{atom,1,libhotswap_dummy},
                                          {atom,1,test},
                                          {integer,1,0}}}] ) ).

%% TEST - Simple function AST extraction from a module AST.
validate_simple__ast_by_mfa2() ->
    FUN = fun libhotswap_util:ast_by_mfa/2,
    {ok,AST} = libhotswap_util:get_ast( libhotswap_dummy ),
    TEST0 = {function,4,test,0,[{clause,4,[],[],[{atom,4,ok}]}]},
    ?assertMatch( {ok, TEST0}, FUN( AST, {libhotswap_dummy, test, 0} ) ).

%% TEST - Make sure it errors out correctly on missing function definition.
validate_missing__ast_by_mfa2() ->
    FUN = fun libhotswap_util:ast_by_mfa/2,
    {ok,AST} = libhotswap_util:get_ast( libhotswap_dummy ),
    ?assertMatch( {error,missing}, FUN( AST, {libhotswap_dummy,missing,0} ) ).

%% TEST - Perform lookup on a standard library function.
validate_stdlibLookup__ast_by_mfa1() ->
    FUN = fun libhotswap_util:ast_by_mfa/1,
    TEST0 = {function,135,pi,0,[{clause,135,[],[],
                                        [{float,135,3.141592653589793}]}]},
    ?assertMatch( {ok, TEST0}, FUN( {math,pi,0} ) ).

%% TEST - Perform lookup on our local dummy module function.
validate_locallibLookup__ast_by_mfa1() ->
    FUN = fun libhotswap_util:ast_by_mfa/1,
    TEST0 = {function,4,test,0,[{clause,4,[],[],[{atom,4,ok}]}]},
    ?assertMatch( {ok, TEST0}, FUN( {libhotswap_dummy, test, 0} ) ).

%% TEST - Injection is suppose to happen when it finds a valid module. It sould
%%        put all attributes after the first export statement.
validate_correctSyntax__inject_attributes2() ->
    FUN = fun libhotswap_util:inject_attributes/2,
    FA = {attribute,1,export,[]},
    Attrs = [FA],
    Module = [module,FA,FA],
    ?assertMatch( {ok, [module,FA,FA,FA]}, FUN( Attrs, Module ) ).

