#!/bin/bash

set -ex

rm -Rf elm-stuff/build-artifacts
rm -Rf elm-stuff/tests/build-artifacts

elm-make --yes
elm-test
