![](https://img.shields.io/badge/STATUS-NOT%20CURRENTLY%20MAINTAINED-red.svg?longCache=true&style=flat)

# Important Notice
This public repository is read-only and no longer maintained.

# BOSH Kubernetes CPI

*NOTE: This documentation is currently valid for the upcoming release v6.*

## Description

This project aims to provide a fully functional BOSH CPI for Kubernetes.

There are limitations considering production readiness.
However, you can in general use Kubernetes as yet another IaaS in the BOSH ecosystem.

You can find details on what is currently possible and what are known limitations below.

## Prerequisites
- [BOSH CLI v2](https://bosh.io/docs/cli-v2.html)

## Installation & Configuration

### Get a cluster

Below are the installation steps for a Minikube and a GCE cluster. The same or similar steps should work on any other cluster.

#### Minikube

- Install [Minikube](https://github.com/kubernetes/minikube#installation)
- Start Minikube
```
$ minikube start --cpus 3 --memory 4096 --disk-size 80g --vm-driver virtualbox
```
- Workaround [Minikube issue #1568](https://github.com/kubernetes/minikube/issues/1568)
```
$ minikube ssh -- sudo ip link set docker0 promisc on
```
- Prepare cluster
```
$ kubectl create -f deployment/minikube/assets/
```
- Create a BOSH environment
```
$ ./deployment/minikube/scripts/create-env.sh
```
- Workaround [Minikube issue #1990](https://github.com/kubernetes/minikube/issues/1990)
```
$ minikube ssh -- chmod 777 /mnt/sda1/hostpath-provisioner/*
```

#### GCE

- Get an account
- Install `gcloud` cli
- Create a cluster
- Prepare cluster
```
$ kubectl create -f deployment/gce/assets/
```
- Wait for the loadbalancer to get an external IP
- Create a BOSH environment
```
$ CLUSTER_NAME=<cluster name> ./deployment/gce/scripts/create-env.sh
```

#### Use the BOSH environment

- Deploy e.g. zookeeper
```
$ cd ~/workspace
$ git clone https://github.com/cppforlife/zookeeper-release.git
$ cd ./deployments/minikube/bosh-env
$ bosh update-cloud-config ../bosh-deployment/kubernetes/cloud-config.yml
$ bosh upload-stemcell https://s3.eu-central-1.amazonaws.com/bosh-kubernetes-cpi-stemcells/bosh-stemcell-3445.11-kubernetes-ubuntu-trusty-go_agent.tgz

$ bosh -d zookeeper deploy ~/workspace/zookeeper-release/manifests/zookeeper.yml
```
- On Minikube
  - Workaround [Minikube issue #1990](https://github.com/kubernetes/minikube/issues/1990)
  ```
  $ minikube ssh -- sudo chmod 777 /mnt/sda1/hostpath-provisioner/*
  ```
- Workaround [Kubernetes CPI issue #2](https://github.com/SAP/bosh-kubernetes-cpi-release/issues/2)
```
$ bosh -d zookeeper deploy ~/workspace/zookeeper-release/manifests/zookeeper.yml
```
- Verify Zookeeper deployment
```
$ bosh -d zookeeper run-errand smoke_tests
```

## Mapping BOSH to Kubernetes

### Virtual Machine
A `Pod` with a single container is used to represent a BOSH *virtual machine*.

### Network
Kubernetes CPI currently only supports *dynamic networks*.

### Persistent Disks
`PersistentVolumeClaim`s are used to represent BOSH *persistent disks*.

## Limitations
- Only *dynamic networks* are supported, hence releases that require static IPs are out for now.
- `attach_disk` is implemented by deleting the Pod and creating a new one with the required *persistent disk* attached.
  - there is currently an issue with BOSH DNS and the recreation of the Pod through `attach_disk`. [#2](https://github.com/SAP/bosh-kubernetes-cpi-release/issues/2)
- bosh agent requires to run in privileged containers
- communication with Kubernetes is always insecure (i.e. no SSL verification)
- legacy authorization
- limited experimentation with nested containers. Installing Garden (and therefore Diego) is probably not possible at the moment
- stemcell is currently taken from https://hub.docker.com/r/loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent/


Copyright and license
---------------------

Copyright (c) 2017 SAP SE

Except as provided below, this software is licensed under the Apache License, Version 2.0 (the "License"); you may not use this software except in compliance with the License.You may obtain a copy of the License at:

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
