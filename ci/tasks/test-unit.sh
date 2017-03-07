#!/bin/bash

set -e

STACK_WORK=$PWD/stack-work

pushd cpi-src/src/bosh-kubernetes-cpi
  cp -R $STACK_WORK/.stack-work .
  stack test
popd
