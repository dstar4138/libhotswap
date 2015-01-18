# LibHotSwap #

LibHotSwap is an Erlang library used to aid in automatic updates of live running 
modules using Erlang's introspection, compiler, and internal module versioning 
mechanism. LibHotSwap can be built into a system to auto update it's own 
functionality based on new information to become specialized or faster, or due 
to the recognition of missing edge-cases and become more generalized.

## How to Use LibHotSwap ##

```erlang
> io:nl(). % print a new line

ok
> NewFunction = fun() -> test end.
#Fun<erl_eval.20.90072148>
> libhotswap:gen_code( NewFunction ).
{ok, "fun()->test end"}
> libhotswap:vsn( io ).
{ok, 220424659779942659805372826583560828129}
> libhotswap:rewrite( true, {io, nl, 0}, NewFunction ).
{ok, 220424659779942659805372826583560828130}
> libhotswap:vsn( modulename ).
{ok, 220424659779942659805372826583560828130}
> io:nl(). % Instead runs the new function as it has been replaced.
test
>
```

## Warning: Write Your Code Correctly ##

LibHotSwap can help you up to a point. But if the function which is getting 
replaced is recursive and it's called like so:

```erlang
f( ... ) -> ... , f( ... ).
```

i.e. standard tail recursion with just the function name; LibHotSwap will not 
be able to patch any running instances of this function. You would need to stop
them manually, and update it to:

```erlang
f( ... ) -> ... , ?MODULE:f( ... ).
```

before LibHotSwap will seamlessly update recursively running functions. Note,
the Erlang VM only keeps two versions of a module in memory. If you disregard 
the above advice, and rewrite the module twice, the third version will knock
out any processes running the oldest version to assuming the processes are
orphaned.


## API ##
```erlang
-type ast()   :: erl_syntax:syntaxTree().
-type func()  :: ast() | string() | fun().
```

#### Add or update code in a module: ####

Note the initial boolean, this is to make check if you want to force the module
update in the event the module is in a Sticky Directory (a safety mechanism 
performed by the Erlang code server for stdlib etc.).

```erlang
add_export( boolean(), mfa(), func() ) -> {ok, vsn()} | {error, term()}.
rewrite( boolean(), mfa(), func() ) -> {ok, vsn()} | {error, term()}.
remove_export( boolean(), mfa() ) -> {ok, vsn()} | {error, term()}.
```

#### Get the Erlang Code (as a string), #### 

Useful if you want to do RegEx replacements before a `rewrite/2`, such as to
replace all calls to `modulev1` to `modulev2`:
```erlang
get_code( term() ) -> {ok, string()} | {error, term()}.
get_code( mfa() ) -> {ok, string()} | {error, term()}.
```

#### Get the Abstract Syntax Tree (as a term): ####

Useful if you want to do more advanced analysis before a `rewrite/2`, such as 
reordering case statements:
```erlang
get_ast( term() ) -> {ok, ast()} | {error, term()}.
get_ast( mfa() ) -> {ok, ast()} | {error, term()}.
```

#### Ease of use module information accessors: ####
```erlang
vsn( ModuleName :: atom() ) -> vsn() | error.
version( ModuleName :: atom() ) -> string() | error.
exports( ModuleName :: atom() ) -> [ mfa() ] | error.
```

#### Some specialized functionality: #### 

To automatically update a single function by affixing, appending, or inserting
some code into it given an function (which can have multiple clauses) based on
a pattern. This pattern specifies which statement location the new code should
appear before, and in which clauses of the function.
```erlang
-type pattern() :: { StatementLocation :: non_neg_integer(), 
                     WhichClauses :: [ non_neg_integer() ] | all }.
-spec inject_in_function( boolean(), mfa(), func(), pattern() ) -> {ok, vsn()} | {error, term()}.
```

Alternatively, you can add a new clause to a function. This may cause an errors
if suddenly have unreachable code.
```erlang
-type order() :: non_neg_integer().
add_new_clause( boolean(), mfa(), func(), order() ) -> {ok, vsn()} | {error, term()}.
```

