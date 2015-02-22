-module(libhotswap_dummy).
-vsn(42).
-export([test/0,test/1,alt/0,alt/1]).
test( ) -> ok.
test(_) -> ok.
alt( ) -> okay.
alt(_) -> okay.
