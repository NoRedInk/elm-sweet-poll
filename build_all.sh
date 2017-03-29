#!/bin/bash

set -ex

rm -Rf elm-stuff/build-artifacts
rm -Rf tests/elm-stuff/build-artifacts
rm -Rf examples/elm-stuff/build-artifacts

elm-make --yes

pushd examples
elm-make --yes
popd

# Tests are currently disabled until elm-testable is upgraded for Elm 0.18
#
# pushd tests
# elm-make --yes --output tests.js TestRunner.elm
# node tests.js
