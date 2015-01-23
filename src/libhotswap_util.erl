%%% libhotswap - Utility Module
%%%
%%% Utility functionality for type conversions and Abstract Syntax Tree 
%%% modification.
%%% 
-module( libhotswap_util ).

-include("libhotswap.hrl").

-export( [check_unsticky/2, funcs/1] ).
-export( [get_beam/1, beam_to_ast/1] ).
-export( [ast_to_code/1, ast_by_mfa/2] ).
-export( [fun_to_ast/1, code_to_ast/1] ).
-export( [inject_attributes/2] ).

%% @doc Check if the module is in a sticky directory (as is the case with stdlib
%%   modules). If it is, and you are want to force it, it'll make it unsticky 
%%   for modification by libhotswap. This is dangerous, for very understandable 
%%   reasons as you will have unknown nondeterministic consequences if forced.
%% @end
-spec check_unsticky( atom(), boolean() ) -> ok | error. 
check_unsticky( Module, Force ) -> 
    case {code:is_sticky( Module ), Force} of
        {true, true} ->
            code:unstick_dir(
                filename:dirname( 
                    code:which( Module )));
        {false, _} -> ok;
        _ -> error
    end.

%% @doc Get the BEAM binary for the module. It does not require the module's
%%   directory be unsticky, but any modifications pushed to the module will.
%% @end
-spec get_beam( atom() ) -> {ok, binary()} | {error, badarg}.
get_beam( Module ) ->
    case code:get_object_code( Module ) of
        {Module, Binary, _Dir} -> {ok, Binary};
        error -> {error, badarg}
    end.

%% @doc Given some BEAM binary, get the Abstract Syntax Tree representation.
-spec beam_to_ast( binary() ) -> {ok, ast()} | {error, badarg}.
beam_to_ast( Binary ) when is_binary( Binary ) ->
    case beam_lib:chunks( Binary, [abstract_code] ) of
        {ok,{_,[{abstract_code,{_,AST}}]}} -> {ok, AST};
        _ -> {error, badarg}
    end.

%% @doc Convert an Erlang Function into it's AST representation. This is useful
%%   for comparisons or to convert known Erlang source into something the 
%%   compiler can work with during injection.
%% @end
-spec fun_to_ast( fun() ) -> {ok, ast()} | {error, badarg | outofscope}.
fun_to_ast( Fun ) -> 
    {env, Tree} = erlang:fun_info( Fun, env ),
    {Env,_Eval,_Val,AST} = lists:last( Tree ),
    case Env of
        [] -> {ok, {'fun',1,{clauses, AST}}};
        _  -> % TODO: Patch AST with values defined out of function scope. 
            {error, outofscope}
    end.

%% @doc Source code to an Abstract Syntax Tree.
-spec code_to_ast( string() ) -> {ok, ast()} | {error, term()}.
code_to_ast( Source ) -> 
    {ok, Ts, _} = erl_scan:string( Source ),
    erl_parse:parse_exprs( Ts ).

%% @doc
-spec funcs( func() ) -> {ok, ast()} | {error, term()}.
funcs( Func ) when is_function( Func ) -> fun_to_ast( Func );
funcs( Func ) ->
    case io_lib:printable_unicode_list( Func ) of
        true -> code_to_ast( Func );
        false -> Func
    end. 

%% @doc Given some Abstract Syntax Tree, get the string representation of it.
-spec ast_to_code( ast() ) -> {ok, string()} | {error, badarg}.
ast_to_code( AST ) ->
    try
        ASTl = case is_list( AST ) of true -> AST; _ -> [AST] end,
        Forms = erl_syntax:form_list( ASTl ),
        String = erl_prettypr:format( Forms ),
        {ok, String}
    catch _:_ -> {error, badarg} end.

%% @doc Given an AST, return the AST for just the function described by the
%%   MFA. It assumes the AST is for the correct moduel. This is used by the 
%%   top-level functionality to highlight the section to get the code/ast for.
%% @end
-spec ast_by_mfa( ast(), mfa() ) -> {ok, ast()} | {error, badarg | missing}.
ast_by_mfa( AST, {_,F,A} ) -> 
    case 
       [ T || {function,_,Fun,Arity,_} = T <- AST, Fun==F, Arity==A ]
    of
        []  -> {error, missing};
        [T] -> {ok, T};
        _   -> {error, badarg}
    end.

%% @doc Inject attributes such as exports/imports/etc. into the top of the 
%%   module.
%% @end
-spec inject_attributes( [Attr], ast() ) -> {ok, ast()} | {error, badarg}
            when Attr :: {attribute, integer(), atom(), term()}.
inject_attributes( Attributes, FullAST ) ->
    case 
        lists:splitwith( fun({attribute, _,_,_}) -> false; 
                            (_) -> true 
                         end, FullAST )
    of
        {[],_} -> {error, badarg};
        {_,[]} -> {error, badarg};
        {T,B}  -> {ok, T++Attributes++B}
    end.

