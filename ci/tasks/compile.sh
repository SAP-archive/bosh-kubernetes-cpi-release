#!/bin/bash

set -e

STACK_WORK_OUTPUT=$PWD/stack-work
export STACK_ROOT=/root/.stack

pushd cpi-src/src/bosh-kubernetes-cpi
  stack build
  cp -R .stack-work $STACK_WORK_OUTPUT/
popd
