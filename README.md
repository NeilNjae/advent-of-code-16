---
title: "Advent of Code 2016"
output: html_document
css: modest.css
---
Code to solve the [Advent of Code](http://adventofcode.com/2016/) puzzles. This year, I'm trying to use the puzzles as a prompt to learn [Haskell](https://wiki.haskell.org/Haskell).

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

I'm using the basic Haskell Platform installation (install with
```
$ sudo aptitude install haskell-platform
```
).

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
ghc -O2 --make advent01.hs -prof -auto-all -caf-all -fforce-recomp
time ./advent01 +RTS -p -hy
```

and create the profile picture with `h2ps advent01.hp` . 

Build this readme file wth
```
pandoc -s README.md > README.html
```

(Using the [Modest styles](https://github.com/markdowncss/modest).)