el-go --- Emacs interface to GO protocols and backends
------------------------------------------------------

- play GO
- interact with GO back ends and protocols through a board
- translate between GO back ends and protocols

A board-based interface to GO games which may be connected to a number
of GO back-ends through a generic API.  To play a game of GO run
`play-go`.  Back-ends include the SGF format, the Go Text Protocol
(GTP), the IGS protocol (in progress).

Usage
-----

1. Load this library.

        (require 'go)
    
2. Play Go.

        (play-go)
