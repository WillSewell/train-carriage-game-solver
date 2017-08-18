#!/bin/bash

rsync --checksum -ave ssh index.html target/trainnumbersolver.js  root@willsewell.com:/var/www/train-num-solver.willsewell.com
