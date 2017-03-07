# BOSH Kubernetes CPI Release

The goal for this project is to provide a fully functional BOSH CPI. It will eventually allow to deploy any BOSH release to any Kubernetes cluster.

##Current status
- [go cli](https://bosh.io/docs/cli-v2.html) can be used to bootstrap a director
- BOSH can be deployed with BOSH
- stemcell is currently taken from https://hub.docker.com/r/loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent/

## Mapping BOSH to Kubernetes

### Virtual Machine
A `Pod` with a single container is used to represent a BOSH *virtual machine*.

### Network
Kubernetes currently only supports *dynamic networks*.

### Persistent Disks
`PersistentVolumeClaim`s are used to represent BOSH *persistent disks*.

## How to use

### Minikube
1. Start minikube with appropriate configuration
```
$ minikube start --cpus 3 --memory 4096 --disk-size 80g --vm-driver xhyve
```
2. Prepare Kubernetes
```
$ kubectl create -f ./minikube/persistent-disks.yml
```

### Bootstrap BOSH
- Deploy an initial BOSH with the new go-cli using templates in bosh-deployment

```
$ mkdir -p ~/projects
$ cd ~/projects
$ cd bosh-deployment
$ git clone git@github.com:loewenstein/bosh-deployment.git
$ git checkout origin/latest-bosh
$ mkdir -p ~/projects/bosh-on-k8s/outer-bosh
$ cd ~/projects/bosh-on-k8s
$ bosh create-env ~/projects/bosh-deployment/bosh.yml \
  --state ./state.json \
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

$ bosh -e $(minikube ip):30555 \
    --ca-cert <(bosh int ./outer-bosh/creds.yml --path /director_ssl/ca) \
    alias-env outer-bosh
```

### BOSH on BOSH
- Upload stemcell and releases
```
$ bosh -e outer-bosh \
    upload-release \
    https://bosh.io/d/github.com/cloudfoundry/bosh?v=261.3 \
    --client admin \
    --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)
$ bosh -e outer-bosh \
    upload-release \
    https://github.com/SAP/bosh-kubernetes-cpi-release/releases/download/v2-alpha/bosh-kubernetes-cpi-2-alpha.tgz \
    --client admin \
    --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)
$ bosh -e outer-bosh \
    upload-stemcell \
    https://github.com/SAP/bosh-kubernetes-cpi-release/releases/download/v1-alpha/bosh-stemcell-3363.9-kubernetes-ubuntu-trusty-go_agent.tgz \
    --client admin \
    --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password)

$ bosh -e outer-bosh \
    update-cloud-config ~/projects/bosh-deployment/kubernetes/cloud-config.yml \
    --client admin \
    --client-secret $(bosh int ./outer-bosh/creds.yml --path /admin_password) \
    -o ~/projects/bosh-deployment/kubernetes/bosh-cloud-config.yml \
    -v port-blobstore=31250 \
    -v port-director=31555 \
    -v port-nats=31422

$ bosh -e outer-bosh \
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
    -v port-nats=31422 \
    --var-file=client-cert=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-certificate"') \
    --var-file=client-key=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-key"')

$ bosh -e $(minikube ip):31555 \
    --ca-cert <(bosh int ./inner-bosh/creds.yml --path /director_ssl/ca) \
    alias-env inner-bosh
```

## Limitations
- Only *dynamic networks* are supported, hence releases that require static IPs are out for now.
- `attach_disk` is implemented by deleting the Pod and creating a new one with the required *persistent disk* attached.
- bosh agent requires to run in privileged containers

Copyright and license
---------------------

Copyright (c) 2017 SAP SE

Except as provided below, this software is licensed under the Apache License, Version 2.0 (the "License"); you may not use this software except in compliance with the License.You may obtain a copy of the License at:

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
