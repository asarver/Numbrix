To load (since the compiled versions are included, you can also load *.o as
opposed to *.lisp):

$gcl
> (load "board_functions.lisp")
> (load "utilities.lisp")
> (load "numbrix.lisp")

To run the interactive version (where the user plays the game):
> (numbrix)

To run the version that solves for you:
> (solve-game)


A program that implements the game called Numbrix. The game is played on a square grid that starts with various positions filled with numbers. The objective of the game is to place numbers into the open cells so they make the path in numerical order. Only horizontal and veritcal paths are allowed.

This program reads in a text file that specifies the current board. The board
size is specified by a number, and its contents are specified by 'row col
number' (where 1,1 is the left bottom corner). Example test files are located
under Tests.

The program then accepts user input in the form of 'row col number'. It will reprint the new board, until the board is completed, and then tell the user if they are correct.
