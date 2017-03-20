#!/bin/bash

set -e

export STACK_ROOT=/root/.stack

pushd cpi-src/src/bosh-kubernetes-cpi
  stack build --test
popd
