-module(fifo_types).
-export([new/0, push/2, pop/1, empty/1]).
-export([test/0]).

-spec new() -> {fifo, [], []}.
new() -> {fifo, [], []}.

-spec push({fifo, In::list(), Out::list()}, term()) -> {fifo, list(), list()}.
push({fifo, In, Out}, X) -> {fifo, [X|In], Out}.

-spec pop({fifo, In::list(), Out::list()}) -> {term(), {fifo, list(), list()}}.
pop({fifo, [], []}) -> erlang:error('empty fifo');
pop({fifo, In, []}) -> pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [H|T]}) -> {H, {fifo, In, T}}.

%-spec empty({fifo, [], []}) -> true;
%           ({fifo, list(), list()}) -> false.
%-spec empty({fifo, [], []}) -> true;
%           ({fifo, [any(), ...], [any(), ...]}) -> false.
-spec empty({fifo, [], []}) -> true;
           ({fifo, [any(), ...], []}) -> false;
           ({fifo, [], [any(), ...]}) -> false;
           ({fifo, [any(), ...], [any(), ...]}) -> false.
empty({fifo, [], []}) -> true;
empty({fifo, _, _}) -> false.

test() ->
    N = new(),
    {2, N2} = pop(push(push(new(), 2), 5)),
    {5, N3} = pop(N2),
    N = N3,
    true = empty(N3),
    false = empty(N2),
    %pop({fifo, [a|b], [e]}).
    pop({fifo, [a, b], [e]}).
