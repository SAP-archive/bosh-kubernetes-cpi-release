#!/bin/bash

set -e

STACK_WORK=$PWD/stack-work

pushd cpi-src/src/bosh-kubernetes-cpi
  stack build
  cp -R .stack_work $STACK_WORK/
popd
