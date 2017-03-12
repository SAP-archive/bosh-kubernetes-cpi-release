#!/bin/bash
set -e


BASE_DIR=${BASE_DIR:-./bosh-on-k8s}
BOSH_DEPLOYMENT=${BOSH_DEPLOYMENT:-$BASE_DIR/bosh-deployment}
echo "Working directory: $BASE_DIR"

mkdir -p bosh-on-k8s
pushd bosh-on-k8s
  if [ ! -d "$BOSH_DEPLOYMENT" ]; then
    echo "Cloning bosh-deployment into: $BOSH_DEPLOYMENT"
    git clone git@github.com:loewenstein/bosh-deployment.git
    pushd bosh-deployment
      git checkout origin/latest-bosh
    popd
  else
    echo "Use existing bosh-deployment directory: $BOSH_DEPLOYMENT"
  fi
  echo "Uploading releases..."
  bosh -e outer-bosh \
      upload-release \
      https://bosh.io/d/github.com/cloudfoundry/bosh?v=261.3 \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)
  bosh -e outer-bosh \
      upload-release \
      https://github.com/SAP/bosh-kubernetes-cpi-release/releases/download/v3-alpha/bosh-kubernetes-cpi-3-alpha.tgz \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)

  echo "Uploading stemcell..."
  bosh -e outer-bosh \
      upload-stemcell \
      https://github.com/SAP/bosh-kubernetes-cpi-release/releases/download/v1-alpha/bosh-stemcell-3363.9-kubernetes-ubuntu-trusty-go_agent.tgz \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)

  echo "Updating cloud-config..."
  bosh -n -e outer-bosh \
      update-cloud-config ~/projects/bosh-deployment/kubernetes/cloud-config.yml \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password) \
      -o $BOSH_DEPLOYMENT/kubernetes/bosh-cloud-config.yml \
      -o $BOSH_DEPLOYMENT/kubernetes/bosh-cloud-config-ports.yml \
      -v port_blobstore=31250 \
      -v port_director=31555 \
      -v port_nats=31422 \
      -v port_ssh_gateway=31022

  mkdir -p inner-bosh
  if [ ! -f inner-bosh/bosh.key.pub ]; then
    ssh-keygen -t rsa -b 4096 -C "vcap" -N "" -f inner-bosh/bosh.key
  fi
  echo "Deploying inner bosh"
  bosh -n -e outer-bosh \
      deploy $BOSH_DEPLOYMENT/bosh.yml \
      -d bosh \
      -o $BOSH_DEPLOYMENT/kubernetes/cpi.yml \
      -o $BOSH_DEPLOYMENT/bosh-dev.yml \
      -o $BOSH_DEPLOYMENT/kubernetes/cpi-inner.yml \
      -o $BOSH_DEPLOYMENT/kubernetes/cpi-inner-ports.yml \
      -o $BOSH_DEPLOYMENT/local-dns.yml \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password) \
      --vars-store ./inner-bosh/creds.yml \
      --var-file=vcap_public_key=inner-bosh/bosh.key.pub \
      -v internal_ip=$(minikube ip) \
      -v director_name=inner-bosh \
      -v port_blobstore=31250 \
      -v port_nats=31422

  echo "Creating env alias: inner-bosh"
  bosh -e $(minikube ip):31555 \
      --ca-cert <(bosh int ./inner-bosh/creds.yml --path /director_ssl/ca) \
      alias-env inner-bosh
