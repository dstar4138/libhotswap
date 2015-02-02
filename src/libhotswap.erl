-module(libhotswap).

-include("libhotswap.hrl").

-export([start_server/0, stop_server/0, check_server/0]).

-export([vsn/1,version/1,exports/1]).
-export([get_code/1,get_ast/1]).
-export([add_export/2,remove_export/1,rewrite/2]).
-export([inject_in_function/3]).
-export([add_new_clause/3]).

-export_type([ast/0, vsn/0, func/0, pattern/0]).


%% ===========================================================================
%% LibHotSwap Server functionality
%% ===========================================================================

%% @doc Start the LibHotSwap Server on the local node as a separate application
%%   tree. To configure the server before start up, please modify the 
%%   application environment via application:set_env/3,4. For the application's
%%   configurations, see src/libhotswap.app.src for details.
%% @end
-spec start_server() -> ok | {error, term()}.
start_server() -> 
    application:start(libhotswap).

%% @doc Stop the LibHotSwap Server. Based on the configurations on startup, this
%%   may have the effect of reverting all modified modules to their original
%%   state.
%% @end
-spec stop_server() -> ok | {error, term()}.
stop_server() ->
    application:stop(libhotswap).

%% @doc Check if the LibHotSwap Server is running. This will return it's pid,
%%   whether it is running as a separate application or part of a larger
%%   supervision tree.
%% @end
-spec check_server() -> {ok, pid()} | false.
check_server() ->
    libhotswap_server:local_instance().

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
    {ok, CleanFAST} = generate_function_from_func( Fun, Arity, AST ),
    NewAST = add_before_eof(ModuleAST, [CleanFAST]),
    {ok, NewModule} = libhotswap_util:inject_attributes( Export, NewAST ),
    reload( MFA, NewModule ).

%% @doc Remove a function which has been exported from the module. This not
%%   only makes it unexported, it will also delete the function code.
%% @end
-spec remove_export( mfa() ) -> {ok, vsn()} | {error, Error}
            when Error :: badarg | badfile | native_code | nofile | 
                          not_purged | novsn | on_load | sticky_directory. 
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
    reload( MFA, Top++[CleanFAST|Bbm] ).


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
inject_in_function( {Module,Fun,Arity}=MFA, Func, Pattern ) -> 
    {ok, FuncAST} = libhotswap_util:funcs( Func ),
    case verify_func_arity( 0, FuncAST ) of
        error -> {error,badarity};
        ok    ->
            io:format("AST:~p~n",[FuncAST]),
            {ok, BodyAST}   = get_ast_body_exprs( FuncAST ),
            {ok, ModuleAST} = libhotswap_util:get_ast( Module ),
            Splitter = find_func(Fun,Arity),
            {Top, [FunAST|Btm]} = lists:splitwith( Splitter, ModuleAST ),
            case pattern_inject( FunAST, BodyAST, Pattern ) of
                {ok, NewFunAST} -> 
                    reload( MFA, Top++[NewFunAST|Btm] );
                Error -> Error
            end
    end.

%% @doc
%%   NOTE: This is very specialized functionality for inclusion in the EMP
%%   application.
%%
%%   Given an MFA, and a Func of the same arity, insert the Func's clauses at
%%   the index position given by the Order. Note this may cause unreachable
%%   code if done incorrectly, such as inserting a match-all case at the 
%%   beginning of the clause structure.
%% @end
-spec add_new_clause( mfa(), func(), non_neg_integer() ) -> {ok, vsn()} | 
                                                            {error, term()}.
add_new_clause( {Module,Fun,Arity}=MFA, Func, Order ) -> 
    {ok, FuncAST} = libhotswap_util:funcs( Func ),
    case verify_func_arity( Arity, FuncAST ) of
        error -> {error,badarity};
        ok    -> 
            {ok, Clauses}   = get_ast_body_clauses( FuncAST ),
            {ok, ModuleAST} = libhotswap_util:get_ast( Module ),
            Splitter = find_func(Fun,Arity),
            {Top, [FunAST|Btm]} = lists:splitwith( Splitter, ModuleAST ),
            case order_inject( FunAST, Clauses, Order ) of
                {ok, NewFunAST} -> 
                    reload( MFA, Top++[NewFunAST|Btm] );
                Error -> Error
            end
    end.

%%% ============
%%% Private
%%% ============

%% @doc Load a new version of a module (MFA) given a new AST. This might be
%%   ripe for improvement (such as with an ondisk cache of all updated 
%%   versions for easy rollback or analysis.
%% @end
-spec reload( module(), ast() ) -> {ok, vsn()} | {error, atom()}.
reload( Module, AST ) -> 
    {ok, NewBinary} = libhotswap_util:ast_to_beam( AST ),
    Fun = case libhotswap_server:local_instance() of
        {ok, _Pid} -> fun libhotswap_server:hotswap/2;
        false      -> fun libhotswap_util:reload/2
    end,
   case Fun( Module, NewBinary ) of
      ok -> {ok, vsn( Module )};
      Error -> Error
    end. 

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
%% @doc Break off the {eof,Line} token and add this to the AST list to the end
%%   correctly (like when we add functions to a module).
%% @end
add_before_eof( ModuleAST, ASTs ) ->
    {Front,EOF} = lists:splitwith( fun({eof,_})->false;
                                      (_)->true 
                                   end , ModuleAST ),
    Front++ASTs++EOF.

%% @private
%% @doc Generate a function AST given a possible 'fun' AST. In otherwords, name
%%   the function. This assumes you are passing in valid function names (atom)
%%   and valid arity for the described clauses.
%% @end
generate_function_from_func( F, A, {'fun',Line,{clauses,Clauses}}=Fun ) ->
    case verify_func_arity( A, Fun ) of
        ok -> {ok, {function,Line,F,A,Clauses}};
        Error -> Error
    end;
generate_function_from_func( F, A, {function,Line,_F,A,Clauses} ) ->
    {ok, {function,Line,F,A,Clauses}};
generate_function_from_func( _, _, _ ) -> {error, badarg}.

%% @private 
%% @doc Check the provided func to see if it is indeed the correct arity. 
verify_func_arity( Arity, {'fun',_,{clauses,[{clause,_,Args,_,_}|_]}} ) 
                         when length(Args)=:=Arity -> ok;
verify_func_arity( Arity, {function,_,_,Arity,_} ) -> ok;
verify_func_arity( _, _ ) -> error.

%% @private
%% @doc Pull out the Expression list from a func. This is used to inject the
%%   func's functionality into another function. Think aspect based programming.
%% @end
get_ast_body_exprs( {'fun',_,{clauses,[{clause,_,[],_,Exprs}]}} ) -> {ok, Exprs};
get_ast_body_exprs( {'function',_,_,0,[{clause,_,[],Exprs}]} )    -> {ok, Exprs};
get_ast_body_exprs( _ ) -> {error, badarg}.

%% @private
%% @doc Pull out the clauses ffrom a Func, for possible insertion into another
%%   function.
%% @end
get_ast_body_clauses( {'fun',_,{clauses,Clauses}} ) -> {ok, Clauses};
get_ast_body_clauses( {'function',_,_,_,Clauses} )  -> {ok, Clauses};
get_ast_body_clauses( _ ) -> {error, badarg}.

%% @private
%% @doc Given a function AST and a set of AST expressions, inject them into the 
%%   the function given a pattern.
%% @end
pattern_inject( FunctionAST, Exprs, {Index,WhichClause} ) ->
    {'function',Line,Name,Arity,Clauses} = FunctionAST,
    Folder = pattern_matcher( Exprs, Index, WhichClause ),
    {_,RNewClauses} = lists:foldl( Folder, {0, []}, Clauses ),
    NewClauses = lists:reverse( RNewClauses ), 
    NewFunctionAST = {'function',Line,Name,Arity,NewClauses},
    {ok, NewFunctionAST}.
is_clause( _, 'all' ) -> true;
is_clause( X, List ) when is_list( List ) -> lists:member(X, List);
is_clause( X, X ) when is_integer(X) -> true;
is_clause( _, _ ) -> false.
pattern_matcher( Exprs, Index, WhichClause ) ->
    InjectorFun = order_injector( Exprs, Index ),
    fun ( Clause, {CurI, RClauses} ) ->
        case is_clause( CurI, WhichClause ) of
           true  ->
             {clause,L,A,Gs,Es} = Clause,
             NewEs = InjectorFun( Es ),
             {CurI+1, [{clause,L,A,Gs,NewEs}|RClauses]};
           false ->
             {CurI+1, [Clause|RClauses]}
        end
    end.
order_injector( Things, 'end' ) -> fun( List ) -> List++Things end;
order_injector( Things, Index ) ->
    fun( List ) -> 
            {Front,Back} = lists:split( Index, List ),
            Front++Things++Back
    end.

%% @private 
%% @doc Given a function AST and a set of AST clauses, inject them via the index
%%   provided by the Order.
%% @end
order_inject( FunctionAST, Clauses, Order ) ->
    try
        {'function',Line,Name,Arity,FunClauses} = FunctionAST,
        InjectorFun = order_injector( Clauses, Order ),
        NewFunClauses = InjectorFun( FunClauses ),
        NewFunctionAST = {'function',Line,Name,Arity,NewFunClauses},
        {ok, NewFunctionAST}
    catch _:Reason -> {error, Reason} end.

