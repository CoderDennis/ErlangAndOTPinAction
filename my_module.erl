-module(my_module).

-export([print/1, area/1, sum/1, do_sum/1, rev/1, tailrev/1]).

print(Term) ->
	io:format("The value of Term is: ~w.~n", [Term]).

area({circle, Radius}) ->
	Radius * Radius * math:pi();
area({square, Side}) ->
	Side * Side;
area({rectangle, Height, Width}) ->
	Height * Width.

sum(0) -> 0;
sum(N) -> sum(N-1) + N.

do_sum(N) -> do_sum(N, 0).

do_sum(0, Total) -> Total;
do_sum(N, Total) -> do_sum(N-1, Total+N).

rev([X | TheRest]) -> 
	rev(TheRest) ++ [X];
rev([])  -> [].

tailrev(List) -> tailrev(List, []).

tailrev([X | TheRest], Acc) -> 
	tailrev(TheRest, [X | Acc]);
tailrev([], Acc) -> Acc.
