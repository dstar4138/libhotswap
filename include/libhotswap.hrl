-type ast()  :: erl_parse:abstract_form().
-type vsn()  :: pos_integer().
-type func() :: string() | fun() | pfunc().
-type pfunc()   :: {'fun',1,_} | {'function',_,_,_,_}.
-type order()   :: non_neg_integer() | 'end'.
-type pattern() :: { order(), all | [ non_neg_integer() ] }.
