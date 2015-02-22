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
modify them before possibly injecting back in. As such, most of LibHotSwap 
only works on modules which have been compiled with the `debug_info` option 
turned on.

```erlang
> TestFun = fun() -> test end.
#Fun<erl_eval.20.90072148>
> libhotswap:get_code( TestFun ). % Convert Erlang to ASTs or Source code.
{ok, "fun () -> test end"}
> libhotswap:get_ast( TestFun ).
{ok, {'fun',0,{clauses,[{clause,1,[],[],[{atom,1,'test'}]}]}}}.
> libhotswap:vsn( io ). % Check Module information like version number
{ok, 220424659779942659805372826583560828129}
> libhotswap:get_ast( fun io:nl/0 ). % Analyze module internals too.
{ok, {function,92,nl,0,
               [{clause,92,[],[],
                        [{call,93,
                               {atom,93,nl},
                               [{call,93,{atom,93,default_output},[]}]}]}]}}
>
```

LibHotSwap also wraps Erlang's natural hot-code-reloading mechanism to provide
more fine-grain module and function-level injection capabilities.

```erlang
> libhotswap_dummy:test(). % just return ok.
ok
> MFA = {libhotswap_dummy, test, 0}.
> libhotswap:inject_in_function( MFA, fun()-> io:format("hi~n") end, {0,[0]} ).
{ok, 43}
> libhotswap_dummy:test(). % Surgically insert code in front of first clause.
hi
ok
> libhotswap:rewrite( MFA, TestFun ). % Brute force, function overwrites
{ok, 43}
> libhotswap_dummy:test(). % Brute force, function overwrite.
test
>
```

However, if you performed two modifications to the same module, only the final
one would take, as the Erlang Code server reads the module's object code from
the path each time LibHotSwap updates the one in memory. LibHotSwap therefore
bundles a server which wraps the code server with an on-disk cache for saving
multiple versions of modifications. This even gives us version rollback
capability:

```erlang
> l(libhotswap_dummy). % Reload from disk.
ok
> libhotswap_dummy:test(). % Now just returns ok again.
ok
> libhotswap:start_server(). % Start libhotswap's code server wrapper
ok
> libhotswap:vsn( libhotswap_dummy ). % Validate version number of our test module
{ok, 42}
> libhotswap:inject_in_function( MFA, fun()-> io:format("hi~n") end, {0,[0]} ).
{ok, 43}.
> libhotswap:inject_in_function( MFA, fun()-> io:format("hi~n") end, {0,[0]} ).
{ok, 44}.
> libhotswap_dummy:test(). % Both injected one in front of the other.
hi
hi
ok
> libhotswap:rollback( libhotswap_dummy ). % Reload the previous version.
{ok, 43}.
> libhotswap_dummy:test().
hi
ok
>
```

There is also a `libhotswap:rollback/2` function provided if you want to roll 
back multiple versions. These functions will fail with an error if the 
LibHotSwap Server is not running.

Also note that LibHotSwap can do this to the standard library or any other
loaded Erlang module. We do not allow this by default (for security sake), but
it can be configured via the application settings before starting the server.

## Integrating LibHotSwap Into an Application ##

LibHotSwap supports [rebar](https://github.com/rebar/rebar). Just import as a
dependency and libhotswap can be used directly as shown above or in the API.

Note some of the features require the LibHotSwap Code Server to be running on 
the node. For example, module version rollbacks and caching changes. Presently,
without the server running you can only inject/overwrite once to a module. To
perform multiple edits, the server needs to have a physical save of each 
successive version.

To turn the server on, and add it to your OTP Application, you can add
`libhotswap_server:start_link/2` to your supervisory tree. Or somewhere in your
application's initialization you can call `application:start(libhotswap)` or 
`libhotswap:start_server()`.

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

