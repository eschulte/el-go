el-go --- Emacs interface to the game of GO
-------------------------------------------

- play GO
- interact with GO back ends and protocols through a board
- translate between multiple GO back ends and protocols

A board-based interface to the game of GO which may be connected to a
number of GO back-ends through a generic API.  Back-ends include the
[Smart Game Format](http://senseis.xmp.net/?SmartGameFormat) (SGF),
the [Go Text Protocol](http://www.lysator.liu.se/~gunnar/gtp/) (GTP)
which may be used to play against gnugo, and the
[Internet Go Server](http://en.wikipedia.org/wiki/IGS_Go_server) (IGS)
protocol (in progress).

Usage
-----

1. Load this library.

        (require 'go)

2. Play Go.

        M-x go-play

3. View an SGF file of a saved game.

        M-x go-view-sgf

4. Connect to the internet go server.  Currently this only supports
   browsing and observing games.

        M-x igs-get-games
