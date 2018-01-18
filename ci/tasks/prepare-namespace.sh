#!/usr/bin/env bash

set -e

source cpi-src/ci/tasks/utils.sh

: {SERVICE_ACCOUNT_KEY:?}
: {PROJECT:?}
: {ZONE:?}
: {CLUSTER:?}

use_service_account $SERVICE_ACCOUNT_KEY
authenticate_cluster $PROJECT $ZONE $CLUSTER

OUTPUT=$PWD/prepare-namespace

NAMESPACE=$(cat <<EOF | kubectl create -f - -o jsonpath={.metadata.name}
---
apiVersion: v1
kind: Namespace
metadata:
  generateName: bats-
  labels:
    purpose: bats
EOF
)

echo $NAMESPACE > $OUTPUT/namespace
echo "Namespace '$NAMESPACE' created"

SERVICE=$(cat <<EOF | kubectl create -f - -o jsonpath={.metadata.name}
---
apiVersion: v1
kind: Service
metadata:
  namespace: $NAMESPACE
  name: bosh-internal
  labels:
    purpose: bats
    run: $NAMESPACE
spec:
  ports:
    - name: blobstore
      port: 25250
      targetPort: 25250
    - name: mbus
      port: 4222
      targetPort: 4222
    - name: director
      port: 25555
      targetPort: 25555
    - name: create-env
      port: 6868
      targetPort: 6868
    - name: ssh
      port: 22
      targetPort: 22
    - name: dns
      port: 53
      targetPort: 53
    - name: uaa
      port: 8443
      targetPort: 8443
    - name: credhub
      port: 8844
      targetPort: 8844
  type: ClusterIP
EOF
)

SERVICE_IP=$(kubectl --namespace=$NAMESPACE get service $SERVICE -o jsonpath={.spec.clusterIP})
echo $SERVICE_IP > $OUTPUT/service_ip
echo "Service '$SERVICE' created with cluster IP $SERVICE_IP"
