#!/bin/bash

set -e

: {STEMCELL_OS:?}

OUTPUT=$PWD/build
INPUT=$PWD

STEMCELL_VERSION=$(cat warden-stemcell/version)
STEMCELL_SHA1=$(cat warden-stemcell/sha1)
FULLNAME=loewenstein/bosh-stemcell-kubernetes-${STEMCELL_OS}-go_agent
LIGHT_STEMCELL_FILENAME=bosh-stemcell-${STEMCELL_VERSION}-kubernetes-${STEMCELL_OS}-go_agent.tgz

mkdir -p $OUTPUT/docker
tar -zxf $INPUT/warden-stemcell/stemcell.tgz image -O > $OUTPUT/docker/image.tgz
cp $PWD/cpi-src/stemcell/agent.json $OUTPUT/docker/agent.json
cat << DOCKERFILE > $OUTPUT/docker/Dockerfile
FROM scratch

ADD  image.tgz /
COPY agent.json /var/vcap/bosh/agent.json
DOCKERFILE

mkdir -p build-stemcell
tar -C build-stemcell --exclude=image -zxf $INPUT/warden-stemcell/stemcell.tgz
pushd build-stemcell
  touch image
  cat << MF_EOF > stemcell.MF
name: bosh-kubernetes-${STEMCELL_OS}-go_agent
version: "$STEMCELL_VERSION"
sha1: $STEMCELL_SHA1
operating_system: ${STEMCELL_OS}
cloud_properties:
  image: "$FULLNAME:$STEMCELL_VERSION"
MF_EOF
  tar -zcf $OUTPUT/$LIGHT_STEMCELL_FILENAME *
popd

echo $STEMCELL_VERSION > $OUTPUT/version