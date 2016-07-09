#!/bin/bash

set -ex

rm -Rf elm-stuff/build-artifacts
rm -Rf elm-stuff/tests/build-artifacts

elm-make --yes

pushd tests
elm-make --yes --output tests.js TestRunner.elm
node tests.js
