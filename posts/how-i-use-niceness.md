# How I use niceness

To stop a process using a lot of CPU and I/O, set its niceness to 19
and its I/O niceness to "idle".

* Directly on the command line

  `ionice -c idle nice -n 19 cabal v2-build` (or whatever)

* Via `top` and `iotop`

  `top`: press `r` to renice a process (to 19)

  `iotop`: press `i` to reionice a process (to idle)
