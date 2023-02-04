# `port_close_stdin`

**Under development.**

Limitation of port closing stdin:

 * https://groups.google.com/g/erlang-programming/c/s6BIzP7I1bw
 * https://erlang.org/pipermail/erlang-questions/2013-July/074916.html
 * https://gist.github.com/timruffles/77e9b69cdecdd7b3ef08
 * https://stackoverflow.com/questions/74833431/use-an-os-process-like-a-bash-pipe-send-it-stdin-and-get-its-stdout
 * https://elixirforum.com/t/rambo-run-your-command-send-eof-get-output/25052/7
 * https://www.erlang.org/bugs/erl-128 (nofix)
 
## Research
 
C integraton:
 
  * https://www.erlang.org/doc/tutorial/c_port.html
  * https://www.erlang.org/doc/tutorial/example.html

Erlang:

 * https://www.erlang.org/doc/man/erlang.html#open_port-2
 * https://www.erlang.org/doc/man/erlang.html#port_close-1

Other:

 * https://www.erlang.org/faq/how_do_i.html
 * https://www.theerlangelist.com/article/outside_elixir
 
Elixir:

 * https://hexdocs.pm/elixir/1.12.2/Port.html

First encounter:

 * http://coolerranch.com/on-ports/
 * https://tonyc.github.io/posts/managing-external-commands-in-elixir-with-ports/
 * https://elixirschool.com/blog/til-ports

Other solutions:

 * https://github.com/jayjun/rambo

## Implementation


Built-in Functions (BIFs)

 * https://erlang.org/pipermail/erlang-questions/2009-October/046899.html
 * https://erlang.org/pipermail/erlang-questions/2008-March/033441.html

Source:

 * https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_bif_port.c#L374
 * https://github.com/erlang/otp/blob/master/erts/emulator/beam/bif.tab

Drivers:

 * https://www.erlang.org/doc/man/driver_entry.html
