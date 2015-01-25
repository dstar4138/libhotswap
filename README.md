# LibHotSwap #

LibHotSwap is an Erlang library used to aid in self-modifying code: easy 
updates of live running modules using Erlang's introspection, compiler, and 
internal module versioning mechanism. LibHotSwap can be built into a larger 
system with goals to become specialized or optimized, via the recognition of 
missing edge-cases with auto updates to it's own code.

LibHotSwap can also be used in the testing of other modules by means of mocking
function behaviour of system. Complex functions can temporarily be overwritten
or whole modules (or single functions) can have functions injected with logging 
information. 

Modules can even take on dynamic natures, with exports being added or removed
during runtime. Think duck-typing where templates can inject functionality on 
modules for compatibility purposes.

**NOTE:** LibHotSwap is currently being developed for use in an event processing
framework called EMP: Extensible Monitoring Platform. It uses it internally to
optimize it's unifying routing function based on frequency of events and the
fluctuation of the user's event-subscription model. Due to this, it maintains
some specialization.

**WARNING:** This library is not being used in production presently. Be very 
careful in considering its use for production purposes. We have not taken the 
time to battle test this nor investigate ease-of-debugging mechanisms yet.

## How to Use LibHotSwap ##

LibHotSwap provides wrappers around Erlang's introspection and compilation 
mechanisms. This is for the purpose of seeing internal representations to 
modify them before possibly injecting back in.

```erlang
> TestFun = fun() -> test end.
#Fun<erl_eval.20.90072148>
> libhotswap:get_code( TestFun ). % Convert Erlang to ASTs or Source code.
{ok, "fun () -> test end"}
> libhotswap:get_ast( TestFun ).
{ok, {'fun',0,{clauses,[{clause,1,[],[],[{atom,1,'test'}]}]}}}.
> libhotswap:vsn( io ). % Check Module information like version number
{ok, 220424659779942659805372826583560828129}
```

LibHotSwap also wraps Erlang's natural hot-code-reloading mechanism to provide
more fine-grain module and function-level injection capabilities.

```erlang
> io:nl(). % print a new line
 
ok
> libhotswap:inject_in_function( {io,nl,0}, fun()-> io:format("hi~n") end, {0,[1]} ).
{ok, 220424659779942659805372826583560828130}.
> io:nl(). % Surgical injection of code in the front of the first function clause.
hi
 
ok
> libhotswap:rewrite( {io, nl, 0}, TestFun ). % Brute force, function overwrites
{ok, 220424659779942659805372826583560828131}
> io:nl(). % Brute force, function overwrite.
test
>
```

## Integrating LibHotSwap Into an Application ##

LibHotSwap supports [rebar](https://github.com/rebar/rebar). Just import as a
dependency and libhotswap can be used directly as shown above or in the API.

Note some of the features require the LibHotSwap Code Server to be running on 
the node. For example, module version rollbacks and caching changes. Presently,
without the server running you can only inject/overwrite once to a module. To
perform multiple edits, the server needs to have a physical save of each 
successive version.

To turn the server on, and add it to your OTP Application, you can add
`libhotswap_server:start_link/2` to your supervisory tree. 

## A Note About Recursive Functions ##

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

## License ##

LibHotSwap is Apache 2.0, please refer to [LICENSE](LICENSE) for more detail.

