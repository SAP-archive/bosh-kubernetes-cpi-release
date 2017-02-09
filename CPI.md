# Writing a CPI (for Kubernetes (in Haskell))

## Documentation

- http://bosh.io/docs/build-cpi.html
  - http://bosh.io/docs/cpi-api-v1.html
  - http://bosh.io/docs/agent-cpi-interactions.html

- http://kubernetes.io

## Basic concepts

### BOSH
- Stemcell
- VM
- Agent
- Registry
- Network
  - dynamic
  - manual
  - vip
- Persistent disks

### Kubernetes
- Container image
- Pod
- Container
- Secret (ConfigMap)
- Network
  - implicit
  - Service
  - access can be limited based on labels (similar to security groups)
- PersistentDisk / PersistentDiskClaim


### Mapping between BOSH and Kubernetes

- VM <-> Pod (wih single container)
- Agent settings can be mounted into the container (i.e. no registry required)
- warden stemcell is good enough as a base for now
  - agent is build from a branch
- minikube default image registry is https://hub.docker.com
  - light stemcells are easy to provide
- manual network not possible (maybe with plugins)
- vip network unclear
  - Service type 'LoadBalancer' supports specifying an IP
  - not sure which provider supports that

## Working with Haskell
Haskell is a statically typed, pure functional programming language.
- explicit effects
- immutable datastructures
- "strange" nomenclature derived from mathematics

- concise syntax for
  - effects
  - datatypes
