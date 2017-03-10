#!/bin/bash

mkdir -p bosh-on-k8s
pushd bosh-on-k8s
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
  bosh -e outer-bosh \
      upload-stemcell \
      https://github.com/SAP/bosh-kubernetes-cpi-release/releases/download/v1-alpha/bosh-stemcell-3363.9-kubernetes-ubuntu-trusty-go_agent.tgz \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)

  bosh -n -e outer-bosh \
      update-cloud-config ~/projects/bosh-deployment/kubernetes/cloud-config.yml \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password) \
      -o ~/projects/bosh-deployment/kubernetes/bosh-cloud-config.yml \
      -v port-blobstore=31250 \
      -v port-director=31555 \
      -v port-nats=31422

  bosh -n -e outer-bosh \
      deploy ~/projects/bosh-deployment/bosh.yml \
      -d bosh \
      -o ~/projects/bosh-deployment/kubernetes/cpi.yml \
      -o ~/projects/bosh-deployment/bosh-dev.yml \
      -o ~/projects/bosh-deployment/kubernetes/cpi-inner.yml \
      -o ~/projects/bosh-deployment/local-dns.yml \
      --client admin \
      --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password) \
      --vars-store ./inner-bosh/creds.yml \
      -v internal_ip=$(minikube ip) \
      -v director_name=inner-bosh \
      -v host_ip=$(minikube ip) \
      -v namespace=default \
      -v port-blobstore=31250 \
      -v port-nats=31422

  bosh -e $(minikube ip):31555 \
      --ca-cert <(bosh int ./inner-bosh/creds.yml --path /director_ssl/ca) \
      alias-env inner-bosh
