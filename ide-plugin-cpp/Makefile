# This assumes al files (*.cpp, *.hpp, doctest.h)
# are located in the same directory as this Makefile!

.PHONY: run

run: binary
	./trie_test

binary:
	clang++ -o trie_test trie_test.cpp --std=c++17
