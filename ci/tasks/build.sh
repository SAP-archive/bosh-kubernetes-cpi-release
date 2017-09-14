#!/bin/bash

set -e

# export STACK_ROOT=/root/.stack
export STACK_ROOT=$PWD/stack-root

pushd cpi-src/src/bosh-kubernetes-cpi
  echo "stack setup..."
  stack setup

  echo
  echo "stack build dependencies..."
  stack build \
    --fast \
    --force-dirty \
    --work-dir=.stack-work-deps \
    --only-dependencies
  cp -R .stack-work-deps .stack-work

  echo
  echo "stack build & test"
  stack build --test
popd
