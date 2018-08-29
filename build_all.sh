#!/bin/bash

set -ex

rm -Rf elm-stuff
rm -Rf examples/elm-stuff

elm make

pushd examples
elm make Main.elm
popd
