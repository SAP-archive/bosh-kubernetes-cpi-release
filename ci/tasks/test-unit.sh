#!/bin/bash

set -e

STACK_WORK_INPUT=$PWD/stack-work
export STACK_ROOT=/root/.stack

pushd cpi-src/src/bosh-kubernetes-cpi
  cp -R $STACK_WORK_INPUT/.stack-work .
  stack test
popd
