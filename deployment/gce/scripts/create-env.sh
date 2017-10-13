#!/bin/bash
set -e

: ${CLUSTER_NAME:?}

BASE_DIR=${1:-$PWD/deployments/gce}
BOSH_DEPLOYMENT=${BOSH_DEPLOYMENT:-$BASE_DIR/bosh-deployment}
echo "Working directory: $BASE_DIR"

KUBE_API_HOST=$(gcloud container clusters describe ${CLUSTER_NAME} --format=json | jq -r .endpoint)
KUBE_TOKEN=$(gcloud config config-helper --format=json | jq -r .credential.access_token)
LOADBALANCER_IP=$(kubectl get svc bosh-external -o jsonpath={.status.loadBalancer.ingress[].ip})

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
  mkdir -p bosh-env
  if [ ! -f bosh-env/bosh.key.pub ]; then
    ssh-keygen -t rsa -b 4096 -C "vcap" -N "" -f bosh-env/bosh.key
  fi
  echo "Creating bosh env"
  bosh create-env \
    --state bosh-env/state.json \
    --vars-store ./bosh-env/creds.yml \
    $BOSH_DEPLOYMENT/bosh.yml \
    -v director_name=gce-env \
    -v internal_ip=$(kubectl get svc bosh-internal -o jsonpath={.spec.clusterIP}) \
    -o $BOSH_DEPLOYMENT/local-dns.yml \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi.yml \
    -v kube_api_server="https://$KUBE_API_HOST" \
    -v kube_namespace=default \
    --var-file=vcap_public_key=bosh-env/bosh.key.pub \
    -o $BOSH_DEPLOYMENT/jumpbox-user.yml \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi-auth-token.yml \
    -v kube_token=$KUBE_TOKEN \
    -o $BOSH_DEPLOYMENT/kubernetes/loadbalancer.yml \
    -v loadbalancer_ip=$LOADBALANCER_IP

  echo "Creating env alias: gce-env"
  bosh -e https://$LOADBALANCER_IP:25555 \
      --ca-cert <(bosh int ./bosh-env/creds.yml --path /director_ssl/ca) \
      alias-env gce-env

  cat <<'EOF' > bosh-env/.envrc
export BOSH_ENVIRONMENT=gce-env
export BOSH_CLIENT=admin
export BOSH_CLIENT_SECRET=$(bosh int ./creds.yml --path /admin_password)
echo $(bosh int ./creds.yml --path /jumpbox_ssh/private_key) > jumpbox.key
chmod 600 jumpbox.key
export BOSH_GW_PRIVATE_KEY=jumpbox.key
EOF
popd
