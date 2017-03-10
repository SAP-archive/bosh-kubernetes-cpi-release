#!/bin/bash

mkdir -p bosh-on-k8s
pushd bosh-on-k8s
  git clone git@github.com:loewenstein/bosh-deployment.git
  pushd bosh-deployment
    git checkout origin/latest-bosh
  popd
  mkdir outer-bosh
  bosh create-env ~/projects/bosh-deployment/bosh.yml \
    --state ./outer-bosh/state.json \
    -o ~/projects/bosh-deployment/kubernetes/cpi.yml \
    -o ~/projects/bosh-deployment/local-dns.yml \
    --vars-store ./outer-bosh/creds.yml \
    -v director_name=outer-bosh \
    -v internal_ip=$(minikube ip) \
    --var-file=client-cert=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-certificate"') \
    --var-file=client-key=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-key"') \
    -v host_ip=$(minikube ip) \
    -v namespace=default \
    -v port-blobstore=30250 \
    -v port-bosh-init=30068 \
    -v port-director=30555 \
    -v port-nats=30422

  bosh -e $(minikube ip):30555 \
      --ca-cert <(bosh int ./outer-bosh/creds.yml --path /director_ssl/ca) \
      alias-env outer-bosh
popd
