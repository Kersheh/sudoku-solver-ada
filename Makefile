# Makefile for Sudoku 

CC = gcc
GNAT = gnatmake
CFLAGS = -c

build:
	$(CC) $(CFLAGS) sudoku.adb
	$(GNAT) sudoku.adb

clean:
	rm -f sudoku *.ali *.o