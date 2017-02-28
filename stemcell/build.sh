#!/usr/bin/env bash
set -e

version=3363.9
url=https://bosh.io/d/stemcells/bosh-warden-boshlite-ubuntu-trusty-go_agent?v=${version}
account=loewenstein
name=bosh-stemcell-$version-kubernetes-ubuntu-trusty-go_agent
fullname=$account/$name

mkdir -p build

echo "Downloading $url"
wget --quiet -O - $url | tar -C build -zx

sha1=$(shasum build/image)
echo "SHA1 in $sha1"

mv build/image build/image.tgz
echo "Building docker image"
docker build -t $fullname:$version . && rm build/image.tgz
docker tag $fullname:$version $fullname:latest

touch build/image

cat << MF_EOF > build/stemcell.MF
name: bosh-kubernetes-ubuntu-trusty-go_agent
version: "$version"
sha1: $sha1
operating_system: ubuntu-trusty
cloud_properties:
  image: "$fullname:$version"
MF_EOF

( cd build && tar -zcf ../bosh-stemcell-${version}-kubernetes-ubuntu-trusty-go_agent.tgz * )

rm -rf build
