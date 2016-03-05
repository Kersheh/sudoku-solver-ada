# Makefile for Sudoku 

CC = gcc
GNAT = gnatmake

DIRS = boards
FILES = $(wildcard $(DIRS:=/*.txt))

build:
	$(CC) -c sudoku.adb
	$(GNAT) -Wall sudoku.adb

test1:
	./sudoku boards/board1.txt

test_boards: boards
	@for file in $(FILES) ; do \
		./sudoku $$file ; \
	done

clean:
	rm -f sudoku *.ali *.o *.txt