#!/usr/bin/env bash

set -e

source cpi-src/ci/tasks/utils.sh

: {SERVICE_ACCOUNT_KEY:?}
: {PROJECT:?}
: {ZONE:?}
: {CLUSTER:?}
: {BOSH_PRIVATE_KEY:?}
: {STEMCELL_OS:?}

use_service_account $SERVICE_ACCOUNT_KEY
authenticate_cluster $PROJECT $ZONE $CLUSTER

PREPARE_NAMESPACE=$PWD/prepare-namespace
DEV_RELEASE=$(ls $PWD/dev-release/bosh-kubernetes-cpi-*\.tgz)
BOSH_DIRECTOR_IP=$(cat $PREPARE_NAMESPACE/service_ip)
BOSH_DEPLOYMENT=$PWD/bosh-deployment-src
BOSH=$PWD/bosh-release
STEMCELL=$(ls $PWD/kubernetes-stemcell/bosh-stemcell-*-kubernetes-${STEMCELL_OS}-go_agent.tgz)
OPS_FILES=$PWD/cpi-src/ci/tasks/ops

OUTPUT=$PWD/create-env

KUBE_API_HOST=$(gcloud container clusters describe $CLUSTER --format=json | jq -r .endpoint)

KEYS=$PWD/keys
mkdir $KEYS
echo -e "$BOSH_PRIVATE_KEY" | tee $KEYS/bosh.key
chmod 600 $KEYS/bosh.key
ssh-keygen -f $KEYS/bosh.key -y > $KEYS/bosh.key.pub

echo "Rendering create-env manifest"
bosh int $BOSH_DEPLOYMENT/bosh.yml \
    --var-errs \
    --vars-store $OUTPUT/creds.yml \
    -v director_name=outer-bosh \
    -v internal_ip=$BOSH_DIRECTOR_IP \
    --var-file=vcap_public_key=$KEYS/bosh.key.pub \
    -o $OPS_FILES/bosh-release-tarball.yml \
    -v bosh_release_tarball=$BOSH/release.tgz \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi.yml \
    -v kube_api_server="https://$KUBE_API_HOST" \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi-auth-token.yml \
    -v kube_token=$(gcloud config config-helper --format=json | jq -r .credential.access_token) \
    -o $OPS_FILES/cpi-release-tarball.yml \
    -v cpi_release_tarball=$DEV_RELEASE \
    -o $OPS_FILES/stemcell-tarball.yml \
    -v stemcell_tarball=$STEMCELL \
    -o $BOSH_DEPLOYMENT/local-dns.yml \
    -v kube_namespace=$(cat $PREPARE_NAMESPACE/namespace) \
    | tee $OUTPUT/create-env.yml

echo "Creating bosh environment"
bosh create-env $OUTPUT/create-env.yml --state $OUTPUT/state.json

bosh -n -e $BOSH_DIRECTOR_IP:25555 \
      --ca-cert <(bosh int $OUTPUT/creds.yml --path /director_ssl/ca) \
      --client admin \
      --client-secret $(bosh int $OUTPUT/creds.yml --path /admin_password) \
      update-runtime-config $BOSH_DEPLOYMENT/runtime-configs/dns.yml
