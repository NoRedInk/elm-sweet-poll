#!/bin/bash

set -ex

rm -Rf elm-stuff/build-artifacts
rm -Rf tests/elm-stuff/build-artifacts
rm -Rf examples/elm-stuff/build-artifacts

elm-make --yes

pushd examples
elm-make --yes Main.elm
popd
