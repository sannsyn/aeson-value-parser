#!/bin/bash
set -eo pipefail

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./.git/*" \
    -not -path "./*.stack-work/*" \
    -not -path "./dist/*" \
    -not -path "./dist-newstyle/*" \
    -not -path "./samples/*" \
    -not -path "./sketches/*" \
    -not -path "./output/*" \
    -not -path "./ideas/*" \
    -not -path "./refs/*" \
    -not -path "./temp/*")
}

function build_and_test {
  stack build \
  --fast --test \
  --ghc-options "-j +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns"
}

format
build_and_test
