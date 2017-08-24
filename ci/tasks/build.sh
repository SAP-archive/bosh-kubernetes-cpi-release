#!/bin/bash

set -e

# export STACK_ROOT=/root/.stack
export STACK_ROOT=$PWD/stack-root

pushd cpi-src/src/bosh-kubernetes-cpi
  stack setup
  stack build --work-dir=.stack-work-deps --only-dependencies
  cp -R .stack-work-deps .stack-work
  stack build --test
popd
