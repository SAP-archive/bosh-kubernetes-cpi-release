#!/bin/bash
set -e


BASE_DIR=${BASE_DIR:-./bosh-on-k8s}
BOSH_DEPLOYMENT=${BOSH_DEPLOYMENT:-$BASE_DIR/bosh-deployment}
echo "Working directory: $BASE_DIR"

mkdir -p $BASE_DIR
pushd $BASE_DIR
  if [ ! -d "$BOSH_DEPLOYMENT" ]; then
    echo "Cloning bosh-deployment into: $BOSH_DEPLOYMENT"
    git clone https://github.com/loewenstein/bosh-deployment.git
    pushd bosh-deployment
      git checkout origin/kubernetes
    popd
  else
    echo "Use existing bosh-deployment directory: $BOSH_DEPLOYMENT"
  fi
  mkdir -p outer-bosh
  if [ ! -f outer-bosh/bosh.key.pub ]; then
    ssh-keygen -t rsa -b 4096 -C "vcap" -N "" -f outer-bosh/bosh.key
  fi
  echo "Deploying outer bosh"
  bosh create-env $BOSH_DEPLOYMENT/bosh.yml \
    --state ./outer-bosh/state.json \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi.yml \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi-ports.yml \
    -o $BOSH_DEPLOYMENT/local-dns.yml \
    --vars-store ./outer-bosh/creds.yml \
    -v director_name=outer-bosh \
    -v internal_ip=$(minikube ip) \
    --var-file=client_cert=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-certificate"') \
    --var-file=client_key=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-key"') \
    --var-file=vcap_public_key=outer-bosh/bosh.key.pub \
    -v kube_api_server="https://$(minikube ip):8443" \
    -v kube_namespace=default \
    -v port_blobstore=30250 \
    -v port_bosh_init=30068 \
    -v port_director=30555 \
    -v port_nats=30422 \
    -v port_ssh_gateway=30022

  echo "Creating env alias: outer-bosh"
  bosh -e $(minikube ip):30555 \
      --ca-cert <(bosh int ./outer-bosh/creds.yml --path /director_ssl/ca) \
      alias-env outer-bosh
popd
