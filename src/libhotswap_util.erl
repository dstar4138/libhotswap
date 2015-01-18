%%% libhotswap - Utility Module
%%%
%%% Utility functionality for type conversions and Abstract Syntax Tree 
%%% modification.
%%% 
-module( libhotswap_util ).

-include("libhotswap.hrl").

-export( [check_unsticky/2] ).
-export( [get_beam/1, beam_to_ast/1] ).
-export( [ast_to_code/1, ast_by_mfa/2] ).

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

%% @doc Convert an Erlang Term into it's AST representation. This is useful
%%   for comparisons or to convert known Erlang source into something the 
%%   compiler can work with during injection.
%% @end
-spec term_to_ast( term() ) -> {ok, ast()} | {error, badarg}.
term_to_ast( Term ) -> ok. 



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

