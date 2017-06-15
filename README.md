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
- Download [Minikube v0.15.0](https://github.com/kubernetes/minikube/releases/tag/v0.15.0)
  - There are problems with newer Minikube / Kubernetes versions that I still have to figure out
- Start minikube with appropriate configuration
```
$ minikube start --cpus 3 --memory 4096 --disk-size 80g --vm-driver virtualbox
```
- Prepare Kubernetes
```
$ kubectl create -f ./minikube/persistent-disks.yml
```

### Bootstrap BOSH
- Deploy an initial BOSH with the new go-cli using templates in bosh-deployment
```
$ ./minikube/scripts/outer-bosh.sh
```

### BOSH on BOSH
- Upload stemcell and releases
- Update cloud-config
- Deploy inner bosh
```
$ ./minikube/scripts/inner-bosh.sh
```

## Limitations
- Only *dynamic networks* are supported, hence releases that require static IPs are out for now.
- `attach_disk` is implemented by deleting the Pod and creating a new one with the required *persistent disk* attached.
- bosh agent requires to run in privileged containers
- communication with Kubernetes is always insecure (i.e. no SSL verification)

Copyright and license
---------------------

Copyright (c) 2017 SAP SE

Except as provided below, this software is licensed under the Apache License, Version 2.0 (the "License"); you may not use this software except in compliance with the License.You may obtain a copy of the License at:

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
