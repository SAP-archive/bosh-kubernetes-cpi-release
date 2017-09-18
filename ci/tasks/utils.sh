#!/usr/bin/env bash

function use_service_account() {
    : {1:?}

    gcloud auth activate-service-account --key-file=<(echo $1 | base64 -d -i)
}

function authenticate_cluster() {
    : {1:?}
    : {2:?}
    : {3:?}
    gcloud config set project $1
    gcloud config set compute/zone $2
    gcloud config set container/cluster $3
    gcloud container clusters get-credentials $3
}

function target_director() {
  : {1:-$PREPARE_NAMESPACE}
  : {2:-$CREATE_ENV}
  export BOSH_ENVIRONMENT=$(cat $1/service_ip)
  export BOSH_CLIENT=admin
  export BOSH_CLIENT_SECRET=$(bosh int $2/creds.yml --path /admin_password)
  bosh int $2/creds.yml --path /director_ssl/ca > ca.cert
  export BOSH_CA_CERT=$PWD/ca.cert
}
