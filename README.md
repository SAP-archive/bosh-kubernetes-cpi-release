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
$ kubectl create -f ./minikube/persistent-disks.yml (for bootstrap BOSH)
$ kubectl create -f ./minikube/persistent-disks.yml (for BOSH on BOSH)
$ kubectl create -f ./minikube/persistent-disks.yml (for any further deployment)
```

### Bootstrap BOSH
- Deploy an initial BOSH with the new go-cli using templates in bosh-deployment

```
$ mkdir -p ~/projects
$ cd ~/projects
$ git clone git@github.com:loewenstein/bosh-deployment.git
$ git checkout origin/kubernetes
$ mkdir -p ~/projects/bosh-on-k8s
$ cd bosh-on-k8s
$ bosh-go create-env ~/projects/bosh-deployment/bosh.yml \
  --state ./state.json \
  -o ~/projects/bosh-deployment/kubernetes/cpi.yml \
  -o ~/projects/bosh-deployment/local-dns.yml \
  --vars-store ./creds.yml \
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
```

### BOSH on BOSH
**This does not work yet**

*Due to the re-creation of the Pod for `attach_disk` there is a timing issue that has to be solved in director before we can deploy with a boostrapped BOSH.*

- Upload stemcell and releases (PoC ignores stemcell, best you choose any *lite stemcell* from https://bosh.io)
```
$ bosh -e $(minikube ip):30555 \
    --ca-cert <(bosh int ./creds.yml --path /director_ssl/ca) \
    alias-env outer-bosh

$ bosh-go -e outer-bosh \
    upload-release \
    https://bosh.io/d/github.com/cloudfoundry/bosh?v=261.2 \
    --client admin \
    --client-secret $(bosh-go int ./creds.yml --path /admin_password)
$ bosh-go -e outer-bosh \
    upload-release \
    ../bosh-kubernetes-cpi.tgz \
    --client admin \
    --client-secret $(bosh-go int ./creds.yml --path /admin_password)
$ bosh-go -e outer-bosh \
    upload-stemcell \
    https://s3.amazonaws.com/bosh-aws-light-stemcells/light-bosh-stemcell-3363.9-aws-xen-hvm-ubuntu-trusty-go_agent.tgz \
    --client admin \
    --client-secret $(bosh-go int ./creds.yml --path /admin_password)

$ bosh -e outer-bosh \
    update-cloud-config ~/projects/bosh-deployment/kubernetes/cloud-config.yml \
    --client admin \
    --client-secret $(bosh int ./creds.yml --path /admin_password) \
    -o ~/projects/bosh-deployment/kubernetes/bosh-cloud-config.yml \
    -v port-blobstore=31250 \
    -v port-director=31555 \
    -v port-nats=31422

$ bosh-go -e outer-bosh \
    deploy ~/projects/bosh-deployment/bosh.yml \
    -d bosh \
    -o ~/projects/bosh-deployment/kubernetes/cpi.yml \
    -o ~/projects/bosh-deployment/bosh-dev.yml \
    -o ~/projects/bosh-deployment/kubernetes/cpi-inner.yml \
    --client admin \
    --client-secret $(bosh-go int ./creds.yml --path /admin_password) \
    --vars-store ./inner-creds.yml \
    -v internal_ip=$(minikube ip) \
    --var-file=client-cert=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-certificate"') \
    --var-file=client-key=$(kubectl config view -o json | jq -r '.users[] | select(.name=="minikube") | .user."client-key"')
```

## Limitations
- Only *dynamic networks* are supported, hence releases that require static IPs are out for now.
- `attach_disk` is implemented by deleting the Pod and creating a new one with the required *persistent disk* attached.
- bosh agent requires to run in privileged containers
