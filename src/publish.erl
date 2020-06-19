-module(publish).

-export([as_cast/2]).

as_cast(Pid, Payload) when is_pid(Pid) ->
    as_cast([Pid], Payload);
as_cast([Head | Tail], Payload) ->
    cast(Head, Payload), as_cast(Tail, Payload);
as_cast([], _Payload) -> ok.

cast(Pid, Payload) -> gen_server:cast(Payload, Pid).
