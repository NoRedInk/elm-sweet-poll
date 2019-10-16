#!/bin/bash

set -ex

rm -Rf elm-stuff
rm -Rf examples/elm-stuff

elm make

pushd examples
elm make Main.elm
popd

# Tests are currently disabled until elm-testable is upgraded for Elm 0.18
#
# pushd tests
# elm-make --yes --output tests.js TestRunner.elm
# node tests.js
