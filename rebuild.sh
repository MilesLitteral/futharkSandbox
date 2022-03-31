#!/bin/bash
futhark cuda fut/entries.fut --library --verbose
futhask fut/entries.json src Futhark

# stack clean
stack build
# cabal v2-build
# cabal v2-run $1
# stack install
# stack run $1
