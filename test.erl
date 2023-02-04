-module(test).
-export([wc/1]).

wc(InputText) ->
    P = open_port({spawn, "wc"}, [stream, exit_status, use_stdio,
                                  stderr_to_stdout, in, out, eof]),
    P ! {self(), {command, InputText}},
    erlang:port_control(P, 16#0112c000, []),
    receive {P, X} -> X end.
