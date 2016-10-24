CC = g++
CPPFLAGS = -g -Wall -O3 -pedantic
#CPPFLAGS = -g -Wall -pg -pedantic -pg
#LDFLAGS = -pg

all: test perf scorer optimize

test: test.o trie.o boggler.o
perf: perf.o trie.o boggler.o
scorer: scorer.o boggler.o trie.o
optimize: optimize.o boggler.o trie.o

clean:
	rm *.o test
