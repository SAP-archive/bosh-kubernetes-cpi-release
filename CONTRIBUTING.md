
# Contributing to bosh-kubernetes-cpi-release

Contributions are highly appreciated. And there are multiple ways you could contribute.
- trying out the CPI and reporting issues and feature requests
- add documentation where it is missing
- contribute code

## Contributor License Agreement
[![CLA assistant check](https://cla-assistant.io/pull/badge/signed)](https://cla-assistant.io/SAP/bosh-kubernetes-cpi-release)

If you plan to contribute changes to this repository you will have to sign the CLA (see badge above).  

## Development

### Workspace setup
- install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- run `stack setup` from inside `./src/bosh-kubernetes-cpi
- Haskell integration is available for Atom, VS Code, Emacs, Vim and JetBrains IDEA (probably more)

### Build & test
```
stack build --test
```
 
