This directory contains a Gnoga web server implementing the game
connect four. It has been contributed by Professors Barry Fagin and Martin
Carlisle of the US Air Force Academy for the JVM-GNAT version.
Adapted to GNOGA by Pascal Pignard.
JVM-GNAT code has been left as comments in order to show the translation to Gnoga.

You can play the game by using your mouse to click on the column where you
want to make your move. After some thinking the program will respond with its
own move. You win if you can align 4 circles in a straight line (vertically,
horizontally, or diagonally). When a winning position is reached you can
restart the game by clicking on the mouse.

Before running connect four, set the language variable for Zanyblue, for instance:
(Replace fr_FR by your own local language)
Unix like: export LANG=fr_FR.UTF-8
Windows: set ZB_LANG=fr_FR.UTF-8
