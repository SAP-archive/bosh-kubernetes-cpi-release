#!/bin/bash

set -e

STACK_WORK=$PWD/stack-work

pushd cpi-src/src/bosh-kubernetes-cpi
  stack build --work-dir $STACK_WORK
popd
