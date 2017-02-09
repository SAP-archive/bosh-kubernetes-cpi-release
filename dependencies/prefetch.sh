# abort script on any command that exits with a non zero value
set -e

CACHE=$PWD/dependencies/cache
BLOBS=$PWD/blobs

pushd $PWD/src/bosh-kubernetes-cpi
  STACK_ROOT=$CACHE stack build --install-ghc --prefetch --dry-run
popd

pushd $CACHE
  tar -czvf $BLOBS/bosh_kubernetes_cpi/dependencies.source.tgz *
popd
