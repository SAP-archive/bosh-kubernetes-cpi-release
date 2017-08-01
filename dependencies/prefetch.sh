# abort script on any command that exits with a non zero value
set -e

DEPENDENCIES=$PWD/dependencies
CACHE=$DEPENDENCIES/cache

pushd $PWD/src/bosh-kubernetes-cpi
  STACK_ROOT=$CACHE stack build --install-ghc --prefetch --dry-run
popd

pushd $CACHE
  rm -rf build-plan-cache programs snapshots
  tar -czvf $DEPENDENCIES/dependencies.source.tgz *
popd
