demoheader
=====

Simple escript to parse Source Engine demo headers.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/demoheader

Usage
-----

    $ demoheader --help
    Usage: demoheader -j [-dnscmgStfl] demo.dem
           demoheader -dnscmgStfl demo.dem
    
      -j, --json              Emit JSON
      -d, --demo-protocol     (version number)
      -n, --network-protocol  (version number)
      -s, --server-name       (IP:port string)
      -c, --client-name       (recorder's in-game name)
      -m, --map-name          (without .bsp extension)
      -g, --game-directory    (tf/, hl2/, etc)
      -S, --playback-seconds  (float)
      -t, --ticks             (integer)
      -f, --frames            (integer)
      -l, --signon-length     (integer)
      -h, --help              Show this help
    
    If -j is not given, exactly one header element must be selected (-dnscmgStfl),
    and it will be printed on standard output. If -j is given, any number of
    header elements may be selected; specifying none is equivalent to specifying
    all of them. Keys are the long forms of their corresponding flags.