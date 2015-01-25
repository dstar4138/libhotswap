-type ast()  :: erl_parse:erl_parse().
-type vsn()  :: pos_integer().
-type func() :: string() | fun() | pfunc().
-type pfunc()   :: {'fun',1,_} | {'function',_,_,_,_}.
-type order()   :: non_neg_integer() | 'end'.
-type pattern() :: { order(), all | [ non_neg_integer() ] }.

% Server Configurations
-type cache_config() :: { DirPath     :: file:filename(),
                          MaxRollback :: non_neg_integer() | false
                        }.
-type replacement_config() :: { GlobalRPC   :: boolean(),
                                AllowStdLib :: boolean(),
                                DefaultPurge:: hard | soft
                              }.
