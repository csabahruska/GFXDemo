#!/bin/sh
ghc --make -O2 Main -Wall -fforce-recomp -fno-warn-unused-do-bind -fno-warn-name-shadowing
