#!/bin/bash

docker run -it --rm -v $PWD:/src rwstauner/haste-compiler hastec --opt-all -o target/trainnumbersolver.js trainnumbersolver.hs
rm Main.jsmod trainnumbersolver.hi trainnumbersolver.o
