#!/bin/bash

set -e

STACK_WORK=$PWD/stack-work

pushd cpi-src/src/bosh-kubernetes-cpi
  stack test --work-dir $STACK_WORK
popd
