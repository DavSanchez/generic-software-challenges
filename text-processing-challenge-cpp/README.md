# Challenge

This is my solution for the challenge. Performed in approximately 3 hours 15 minutes.

## Challenge briefing

Develop an application so that given two lists of phrases (string of ASCII characters forming words) A and B, finds in which phrase of list B appear all the words contained in each one of the phrases of list A. For example, if all the words of phrase in row 5 of list A appear in phrase located in row 11 of List B, a “A5 B11” output string must be given, and so on for each one of the phrases of list A.

The lists A and B will be read from a single ASCII file. Each phrase will be in a different line (that is to say, there is a carriage return at the end of the line). The list A will begin immediately after the line with the marker "LISTA A"; the same for the list B, whose marker is "LISTA B". The first phrase for each list will be considered phrase 0.

The comparison will be considered as non case-sensitive. Also, for comparison purposes, it will be considered that acute vocals (áéíóúÁÉÍÓÚ) and non-acute vocals are the same.

A “word” will be defined as a contiguous sequence of letters or numbers. Therefore, each word will be delimited (previous and later) by spaces and/or special characters (like punctuation), except the first and last words of each phrase that will be delimited by either: (a) the beginning of a line or (b) EOF or a change of line.

The results will be saved to an ASCII output file RESULTS.txt.

The data structures for each phrase and the lists of phrases will have to be allocated dynamically and required memory has to increase as needed at run time. A balance between the efficiency of the code (execution speed of the application) and consumption of memory must be considered.

For performance purposes, the programmer must assume both lists of phrases are very long.

## Building (with `meson`)

```console
$ meson setup build
The Meson build system
Version: 0.61.1
Source dir: /<local_path>/text-processing-challenge
Build dir: /<local_path>/text-processing-challenge/builddir
Build type: native build
Project name: text-processing-challenge
Project version: undefined
C++ compiler for the host machine: c++ (clang 13.0.0 "Apple clang version 13.0.0 (clang-1300.0.29.30)")
C++ linker for the host machine: c++ ld64 711
Host machine cpu family: aarch64
Host machine cpu: arm64
Build targets in project: 1

Found ninja-1.10.2 at /opt/homebrew/bin/ninja

$ cp ./INPUT.txt ./builddir/
$ cd builddir
$ meson compile
[2/2] Linking target text-processing-challenge
$ ./text-processing-challenge
[INFO] Reading INPUT.txt
[INFO] Generating output file RESULTS.txt
[INFO] Done!
```

## Why a single file?

Given that the solution is less than 150 lines of code with only 4 functions, I considered better to have it in just one file and avoid losing time setting up a more complex `makefile` or `meson.build`. Also, I wanted to have a quickstart with `meson` but I didn't want to lose a lot of the allocated time with it, hence the minimum complications regarding source files.
