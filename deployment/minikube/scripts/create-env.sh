#!/bin/bash
set -e

BASE_DIR=${1:-$PWD/deployments/minikube}
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
  mkdir -p bosh-env
  if [ ! -f bosh-env/bosh.key.pub ]; then
    ssh-keygen -t rsa -b 4096 -C "vcap" -N "" -f bosh-env/bosh.key
  fi
  echo "Creating bosh env"
  bosh create-env \
    --state bosh-env/state.json \
    --vars-store ./bosh-env/creds.yml \
    $BOSH_DEPLOYMENT/bosh.yml \
    -v director_name=minikube-env \
    -v internal_ip=$(kubectl get svc bosh-internal -o jsonpath={.spec.clusterIP}) \
    -o $BOSH_DEPLOYMENT/local-dns.yml \
    -o $BOSH_DEPLOYMENT/kubernetes/cpi.yml \
    --var-file=client_cert=$(kubectl config view -o json | jq -r '.users[] | select(.name=="'minikube'") | .user."client-certificate"') \
    --var-file=client_key=$(kubectl config view -o json | jq -r '.users[] | select(.name=="'minikube'") | .user."client-key"') \
    --var-file=vcap_public_key=bosh-env/bosh.key.pub \
    -v kube_api_server="https://$(minikube ip):8443" \
    -v kube_namespace=default \
    -o $BOSH_DEPLOYMENT/kubernetes/node-port.yml \
    -v node_ip=$(minikube ip) \
    -v create_env_port=30068

  echo "Creating env alias: minikube-env"
  bosh -e $(minikube ip):30555 \
      --ca-cert <(bosh int ./bosh-env/creds.yml --path /director_ssl/ca) \
      alias-env minikube-env

  cat <<'EOF' > ./bosh-env/.envrc
export BOSH_ENVIRONMENT=minikube-env
export BOSH_CLIENT=admin
export BOSH_CLIENT_SECRET=$(bosh int ./creds.yml --path /admin_password)
EOF
popd
