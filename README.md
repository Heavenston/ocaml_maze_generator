# OCaml Maze Generator

Welcome to my OCaml Maze Generator, a... maze generator... written in OCaml.

## How it works

It uses the "randomized depth-first"  algorithm, more info on it on [wikipedia](https://en.wikipedia.org/wiki/Maze_generation_algorithm#Randomized_depth-first_search). I chosed it because it makes mazes that has the look and feel out of any algorithm i could find online/come up with.

## Usage

    maze_generator.exe --help

## How to compile
Make sure you have `opam` and `dune` installed and setup, then install all the depdencies:

    dune install bimage bimage-io bimage-unix clap

then go inside this project directory and run:
	
	dune build
	
This will create the executable on `_build/default/maze_generator.exe`, if you want to directly run the program, type:

    dune exec ./maze_generator.exe -- [(--width|-w) <VALUE>] [(--height|-h) <VALUE>] [<OUTPUT>]

