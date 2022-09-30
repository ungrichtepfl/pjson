#!/bin/bash

which ghc >> /dev/null || (echo "Please install ghc."; exit 1)

echo "Removing build artefacts..."
rm -f Main.hi Main.o pjson
echo "Building \"pjson\"..."
ghc Main.hs -o pjson
echo "Run \"/.pjson filename\"."
