-type ast()  :: erl_syntax:syntaxTree().
-type vsn()  :: pos_integer().
-type func() :: ast() | string() | fun().
-type pattern() :: { non_neg_integer(), all | [ non_neg_integer() ] }.

