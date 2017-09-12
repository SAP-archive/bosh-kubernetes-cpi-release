#!/usr/bin/env bash

set -e

PREPARE_NAMESPACE=$PWD/prepare-namespace
CREATE_ENV=$PWD/create-env

# path to the stemcell you want to use for testing
export BAT_STEMCELL=$(ls $PWD/kubernetes-stemcell/bosh-stemcell-*-kubernetes-ubuntu-trusty-go_agent.tgz)

KEYS=$PWD/keys
mkdir $KEYS
ssh-keygen -t rsa -b 4096 -C "vcap" -N "" -f $KEYS/bats.key
echo "private key used for BATs"
cat $KEYS/bats.key
# the path to ssh key, used by OS specs to ssh into BOSH VMs
export BAT_PRIVATE_KEY=$KEYS/bats.key
export BAT_PUBLIC_KEY=$KEYS/bats.key.pub

# path to the bat yaml file which is used to generate the deployment manifest (see below `bat.yml`)
export BAT_DEPLOYMENT_SPEC=$PWD/bats-config.yml
cat > $BAT_DEPLOYMENT_SPEC <<EOF
---
cpi: kubernetes
properties:
  use_static_ip: false
  vip: $(cat $PREPARE_NAMESPACE/service_ip)
  uuid: d5ce767a-b44e-4ec6-af72-38c098b0e032
  pool_size: 1
  instances: 1
  stemcell:
    name: "bosh-kubernetes-ubuntu-trusty-go_agent"
    version: latest
  networks:
    - name: default
      type: dynamic
      cloud_properties: []
  password: "$6yRjvxit4D4Y"
  vcap_public_key: "$(cat $KEYS/bats.key.pub)"
EOF

# BOSH CLI executable path
export BAT_BOSH_CLI=/usr/local/bin/bosh2

# DNS host or IP where BOSH-controlled PowerDNS server is running, which is required for the DNS tests. For example, if BAT is being run against a MicroBOSH then this value will be the same as BAT_DIRECTOR
export BAT_DNS_HOST=$(cat $PREPARE_NAMESPACE/service_ip)

# the name of infrastructure that is used by bosh deployment. Examples: aws, vsphere, openstack, warden.
export BAT_INFRASTRUCTURE=kubernetes

# the type of networking being used: `dynamic` or `manual`.
export BAT_NETWORKING=dynamic

# Run tests with --fail-fast and skip cleanup in case of failure (optional)
# export BAT_DEBUG_MODE=false

export BOSH_OS_BATS=false

export BOSH_ENVIRONMENT=$(cat $PREPARE_NAMESPACE/service_ip)
export BOSH_CLIENT=admin
export BOSH_CLIENT_SECRET=$(bosh int $CREATE_ENV/creds.yml --path /admin_password)
bosh int $CREATE_ENV/creds.yml --path /director_ssl/ca > ca.cert
export BOSH_CA_CERT=$PWD/ca.cert

echo "using bosh CLI version..."
bosh --version

cd bats
bundle install -j4
bundle exec rspec --tag ~manual_networking --tag ~raw_ephemeral_storage --tag ~dns --tag ~migrate_disk spec
