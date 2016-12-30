---
title: "Advent of Code 2016"
output: html_document
css: modest.css
---
Code to solve the [Advent of Code](http://adventofcode.com/2016/) puzzles. This year, I'm trying to use the puzzles as a prompt to learn [Haskell](https://wiki.haskell.org/Haskell).

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

# Toolchain

I'm using the basic Haskell Platform installation, togeher with `Stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
).

I have one package for each day, to save time waiting for Stack to check every executable before compiling what's changed. Each package needs a separate directory tree and a separate `.cabal` file. 

Compile with
```
stack build
```
or 
```
stack build adventofcode1601
```

Run with
```
stack exec advent01
```

Run interactively with
```
stack ghci adventofcode1601:exe:advent01
```

To profile, use 
```
stack build --executable-profiling --library-profiling -ghc-options="-fprof-auto -rtsopts" adventofcode1601
```
then run with
```
stack exec -- advent01 +RTS -p -hy
```

# Readme

Build this readme file wth
```
pandoc -s README.md > README.html
```

### Earlier instructions, for compiling before use of Stack

I'm also using some extra libraries. Before installing, run `cabal update` then set `library-profiling: True` in `~/.cabal/config` . Then install the packages with  
```
$ cabal install MissingH
$ cabal install parsec-numbers
$ cabal install cryptonite
$ cabal install pqueue
```

Compile the code with
```
ghc --make advent01.hs
```

then run it as 
```
advent01
```

If you're profiling, compile and run with 
```
ghc -O2 --make advent01.hs -prof -auto-all -caf-all -fforce-recomp -rstopts
time ./advent01 +RTS -p -hy
```

and create the profile picture with `h2ps advent01.hp` . 

(Using the [Modest style](https://github.com/markdowncss/modest).)
