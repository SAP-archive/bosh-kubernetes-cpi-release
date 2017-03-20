#!/bin/bash

set -e

DEV_RELEASE=$PWD/dev-release
semver=`cat dev-version-semver/number`

pushd cpi-src
  bosh create-release \
    --version $semver \
    --tarball $DEV_RELEASE/bosh-kubernetes-cpi.$semver.tgz
popd
