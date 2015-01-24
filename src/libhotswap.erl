-module(libhotswap).

-include("libhotswap.hrl").

-export([vsn/1,version/1,exports/1]).
-export([get_code/1,get_ast/1]).
-export([add_export/2,remove_export/1,rewrite/2]).
-export([inject_in_function/3]).
-export([add_new_clause/3]).

-export_type([ast/0, vsn/0, func/0, pattern/0]).


%% ===========================================================================
%% Misc Ease of Access Functionality
%% ===========================================================================

%% @doc Get the VSN of a loaded module.
-spec vsn( atom() ) -> {ok, pos_integer()} | {error, term()}.
vsn( ModuleName ) when is_atom( ModuleName ) -> 
    MI = erlang:apply( ModuleName, module_info, [] ),
    case 
        catch [ VS || {attributes, As} <- MI, {vsn,[VS]} <- As ]
    of
        [VS] when is_integer(VS) -> {ok, VS};
        [] -> {error, novsn};
        _  -> {error, badarg}
    end.

%% @doc Get the developer provided version string of the module.
-spec version( atom() ) -> {ok, string()} | {error, term()}.
version( ModuleName ) when is_atom( ModuleName ) -> 
    MI = erlang:apply( ModuleName, module_info, [] ),
    case 
        catch [ VS || {compile, Cs} <- MI, {version, VS} <- Cs ]
    of
        [VS] when is_list(VS) -> {ok, VS};
        [] -> {error, noversion};
        _  -> {error, badarg}
    end.

%% @doc Get the list of exports for a provided module.
-spec exports( atom() ) -> {ok, [mfa()]} | {error, term()}.
exports( ModuleName ) when is_atom( ModuleName )  ->  
    case 
        erlang:apply( ModuleName, module_info, [ exports ] )
    of
        Es when is_list( Es ) -> {ok, Es};
        _                     -> {error, badarg}
    end.

%% ===========================================================================
%% Code Accessor Functionality
%% ===========================================================================

%% @doc Get the Raw Erlang code from a loaded BEAM module. This is useful if
%%   you want to do RegEx replacements before a `rewrite/2`, e.g. replace all
%%   calls to `modulev1` to `modulev2`. NOTE: if the Fun() is a closure (i.e.
%%   variables are referenced which are created external to the Fun() itself
%%   then this function will return an error.
%% @end
-spec get_code( mfa() | fun() ) -> {ok, string()} | {error, Error}
            when Error :: 'badarg'    | % term is not an mfa/fun.
                          'missing'   | % mfa() could not be found.
                          'outofscope'. % fun() has a non-empty reference env.
get_code( Term ) -> 
    case get_ast( Term ) of
        {ok, AST} -> libhotswap_util:ast_to_code( AST );
        Error     -> Error
    end.        

%% @doc Get the Abstract Syntax Tree from a loaded BEAM module. This is useful
%%   if you want to do more advanced analysis before a `rewrite/2`, e.g. 
%%   reordering case statements.
%% @end
-spec get_ast( mfa() | term() ) -> {ok, ast()} | {error, Error}
            when Error :: 'badarg'    | % term is not an mfa/fun.
                          'missing'   | % mfa() could not be found.
                          'outofscope'. % fun() has a non-empty reference env.
get_ast( {Module, _, _} = MFA ) -> 
    case libhotswap_util:get_ast( Module ) of
        {ok, AST} -> libhotswap_util:ast_by_mfa( AST, MFA );
        Error -> Error
    end; 
get_ast( ErlOrCode ) -> libhotswap_util:funcs( ErlOrCode ).


%% ===========================================================================
%% Module Manipulation
%% ===========================================================================


%% @doc Add a new function to a module and load it back into memory.
-spec add_export( mfa(), func() ) -> {ok, vsn()} | {error, term()}. 
add_export( {Module,Fun,Arity}=MFA, Func ) ->
    {ok, AST} = libhotswap_util:funcs( Func ),
    {ok, ModuleAST} = libhotswap_util:get_ast( Module ),
    Export = [{attribute, 1, export, [{Fun, Arity}]}],
    NewModule = libhotswap_util:inject_attributes( Export, ModuleAST++[AST] ),
    reload( MFA, NewModule ).

%% @doc Remove a function which has been exported from the module. This not
%%   only makes it unexported, it will also delete the function code.
%% @end
-spec remove_export( mfa() ) -> {ok, vsn()} | {error, term()}.
remove_export( {Module,F,A}=MFA ) ->
    {ok, ModuleAST} = libhotswap_util:get_ast( Module ),
    {Top, [Ex|Btm]} = lists:splitwith( find_attr(F,A), ModuleAST ),
    {BTop,[_F|Bbm]} = lists:splitwith( find_func(F,A), Btm ),
    {attribute, Line, export, Exs} = Ex,
    CleanExs = lists:delete( {F,A}, Exs ),
    NewModule = Top++[{attribute,Line,export,CleanExs}]++BTop++Bbm,
    reload( MFA, NewModule ).

%% @doc Given a func and an MFA, overwrite the MFA with the provided Func.
%%   This will completely overload the function and reload the module. Be
%%   very careful doing this to stdlib functions.
%% @end
-spec rewrite( mfa(), func() ) -> {ok, vsn()} | {error, term()}.
rewrite( {Module,F,A}=MFA, Func ) -> 
    {ok, FunctnAST} = libhotswap_util:funcs( Func ),
    {ok, ModuleAST} = libhotswap_util:get_ast( Module ),
    {Top, [_F|Bbm]} = lists:splitwith( find_func(F,A), ModuleAST ),
    {ok, CleanFAST} = generate_function_from_func( F,A, FunctnAST ),
    reload( MFA, Top++Bbm++CleanFAST ).

%% ===========================================================================
%% Specialized functionality
%% ===========================================================================

%% @doc 
%%   NOTE: This is very specialized functionality for inclusion in the EMP
%%   application. 
%%
%%   Given a reference MFA, add the body of Func to it according to a
%%   provided pattern. Namely, how far down the expression list for which 
%%   of the functions clauses. For example; if the pattern was: {2, all}
%%   the result would be to add the body of Func after the second line of
%%   all clauses in MFA. If the pattern is {'end', [3,5]}, it will add the
%%   body of Func at the end of the third and fifth clauses of MFA.
%% @end
-spec inject_in_function( mfa(), func(), pattern() ) -> {ok, vsn()} | 
                                                        {error, term()}.
inject_in_function( MFA, Func, Pattern ) -> 
    {ok, FuncAST} = libhotswap_util:funcs( Func ),
    case verify_func_arity( 0, FuncAST ) of
        error -> {error,badarity};
        ok    ->
            %TODO: find MFA, inject based on pattern.
            ok
    end.

%% @doc
%%   NOTE: This is very specialized functionality for inclusion in the EMP
%%   application.
%%
%%   Given an MFA, 
%%
%% @end
-spec add_new_clause( mfa(), func(), non_neg_integer() ) -> {ok, vsn()} | {error, term()}.
add_new_clause( MFA, Func, Order ) -> ok.


%%% ============
%%% Private
%%% ============

%% @private
%% @doc Returns an AST splitter which attempts to find the exported MFA. 
find_attr( Fun, Attr ) -> 
    fun( {attribute, _Line, export, Exs} ) -> not lists:member({Fun,Attr}, Exs);
       ( _ ) -> true
    end.

%% @private
%% @doc Returns an AST splitter which attempts to find the function by MFA.
find_func( Fun, Attr ) ->
    fun( {function,_Line,F,A,_} ) when F=:=Fun andalso A=:=Attr -> false;
       ( _ ) -> true
    end.

%% @private
%% @doc Generate a function AST given a possible 'fun' AST. In otherwords, name
%%   the function. This assumes you are passing in valid function names (atom)
%%   and valid arity for the described clauses.
%% @end
generate_function_from_func( F, A, {'fun',Line,Clauses}=Fun ) ->
    case verify_func_arity( A, Fun ) of
        ok -> {ok, [{function,Line,F,A,Clauses}]};
        Error -> Error
    end;
generate_function_from_func( F, A, {function,Line,_F,A,Clauses} ) ->
    {ok, [{function,Line,F,A,Clauses}]};
generate_function_from_func( _, _, _ ) ->
    {error, badarg}.

%% @private 
%% @doc Check the provided func to see if it is indeed the correct arity. 
verify_func_arity( Arity, {'fun',_,[{clause,_,Args,_,_}|_]} ) 
                         when length(Args)=:=Arity -> ok;
verify_func_arity( Arity, {function,_,_,Arity,_} ) -> ok;
verify_func_arity( _, _ ) -> error.

%% Load a new version of a module (MFA) given a new AST.
-spec reload( mfa(), ast() ) -> {ok, vsn()} | {error, atom()}.
reload( MFA, AST ) -> ok.
