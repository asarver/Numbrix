To run:

$gcl
> (load "numbrix.lisp")
> (numbrix)


Test cases are located under Tests.

A program that implements the game called Numbrix. The game is played on a square grid that starts with various positions filled with numbers. The objective of the game is to place numbers into the open cells so they make the path in numerical order. Only horizontal and veritcal paths are allowed.

As of right now, this program reads in a text file that specifies the current board. The board size is specified by a number, and its contents are specified by 'row col number' (where 1,1 is the left bottom corner). An example text file with a board of size 8x8 would look like:

8
8 1 45
8 2 44
8 3 39
8 4 38
8 5 23
8 6 22
8 7 19
8 8 18
7 1 46
6 1 47
5 1 48
4 1 63
3 1 64
2 1 59
1 1 58
1 2 57
1 3 56
1 4 55
1 5 8
1 6 7
1 7 6
1 8 5
2 8 4
3 8 3
4 8 14
5 8 15
6 8 16
7 8 17

The program then accepts user input in the form of 'row col number'. It will reprint the new board, until the board is completed, and then tell the user if they are correct.



