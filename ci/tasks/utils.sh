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