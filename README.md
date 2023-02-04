# Send EOF to Port

**Under development.**

## Problem Statement

Limitation of port closing stdin:

 * https://groups.google.com/g/erlang-programming/c/s6BIzP7I1bw
 * https://erlang.org/pipermail/erlang-questions/2013-July/074916.html
   * http://erlang.org/pipermail/erlang-questions/2010-November/054330.html
   * http://erlang.org/pipermail/erlang-questions/2010-October/053944.html
   * http://erlang.org/pipermail/erlang-questions/2009-March/042123.html
   * http://stackoverflow.com/questions/8792376/erlang-ports-interfacing-with-a-wc-like-program
 * https://gist.github.com/timruffles/77e9b69cdecdd7b3ef08
 * https://stackoverflow.com/questions/74833431/use-an-os-process-like-a-bash-pipe-send-it-stdin-and-get-its-stdout
 * https://elixirforum.com/t/rambo-run-your-command-send-eof-get-output/25052/7
 * https://www.erlang.org/bugs/erl-128 (nofix)

Hacks:

 * https://github.com/mattsta/erlang-stdinout-pool#why-is-this-special
 * https://github.com/saleyn/erlexec

Workaround:

 * https://erlang.org/pipermail/erlang-questions/2013-July/074937.html

> And a program that waits for all of its input before producing any data is by definition not a filter.
> 
> There is a very very simple technique.
> (1) Create a temporary file.
> (2) Create a pipe, telling that pipe to write to the temporary file.
> (3) Send your data to the pipe and close the pipe.
> (4) Now read the temporary file.



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
 * https://www.erlang.org/doc/apps/erts/driver.html
 * https://www.erlang.org/doc/tutorial/c_portdriver.html

[`typedef struct erl_drv_entry`](https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_driver.h#L215)

```
erts/emulator/sys/unix/sys_drivers.c:233:struct erl_drv_entry spawn_driver_entry = {
erts/emulator/sys/unix/sys_drivers.c:259:struct erl_drv_entry fd_driver_entry = {
erts/emulator/sys/unix/sys_drivers.c:286:struct erl_drv_entry vanilla_driver_entry = {
erts/emulator/sys/unix/sys_drivers.c:313:struct erl_drv_entry forker_driver_entry = {
```

Ports:

 * https://www.erlang.org/doc/reference_manual/ports.html
 * https://www.erlang.org/doc/man/erlang.html#port_control-3

UNIX:

 * https://man7.org/linux/man-pages/man3/exec.3.html

## Solution

### Possibile Solution 1

Implement a control to be called from `port_control/3` in the approproate driver to close the write file descriptor, which will signal EOF.
