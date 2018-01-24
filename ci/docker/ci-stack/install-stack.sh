#!/usr/bin/env bash

STACK_VERSION=${STACK_VERSION:="1.6.3"}
STACK_URL=https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz

apt-get install -y \
     g++ \
     gcc \
     libc6-dev \
     libffi-dev \
     libgmp-dev \
     make \
     xz-utils \
     zlib1g-dev \
     git \
     gnupg
apt-get clean

mkdir -p /tmp/stack
pushd /tmp/stack
  wget -O - ${STACK_URL} | tar -zx
  chmod +x stack-*/stack
  mv stack-*/stack /usr/local/bin/stack
popd