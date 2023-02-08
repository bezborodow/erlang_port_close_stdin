-module(test).
-export([wc/1]).

wc(InputText) ->
    P = open_port({spawn, "wc"}, [stream, use_stdio]),
    P ! {self(), {command, InputText}},
    port_control(P, 16#0112c000, []),
    receive {P, X} -> X end.
