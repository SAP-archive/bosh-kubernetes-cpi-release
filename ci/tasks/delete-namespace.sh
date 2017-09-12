#!/usr/bin/env bash

set -e

source cpi-src/ci/tasks/utils.sh

: {SERVICE_ACCOUNT_KEY:?}
: {PROJECT:?}
: {ZONE:?}
: {CLUSTER:?}

use_service_account $SERVICE_ACCOUNT_KEY
authenticate_cluster $PROJECT $ZONE $CLUSTER

PREPARE_NAMESPACE=$PWD/prepare-namespace

NAMESPACE=$(cat $PREPARE_NAMESPACE/namespace)

echo "Deleting namespace '$NAMESPACE' ..."
kubectl delete namespace $NAMESPACE
echo "Namespace '$NAMESPACE' deleted"
