#!/bin/bash

set -e

cd src

make Browser
if [[ -e "Browser.gz" ]] ; then
  rm Browser.gz
fi
gzip Browser
scp Browser.gz wmflabs:/data/project/flossbrowser/
rm Browser.gz

make Main
./Main
scp flossbrowser.sqlite wmflabs:/data/project/flossbrowser/
