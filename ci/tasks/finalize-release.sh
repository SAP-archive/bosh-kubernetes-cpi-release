#!/usr/bin/env bash

set -e -x

: {aws_access_key_id:?}
: {aws_secret_access_key:?}
: {release_version_suffix:=""}

DEV_RELEASE=$PWD/dev-release

# Creates an integer version number from the semantic version format
# May be changed when we decide to fully use semantic versions for releases
major_version=`cut -d "." -f1 release-version-semver/number`
version="${major_version}${release_version_suffix}"
echo "${version}" > promote/version

cp -r cpi-src promote/repo

cd promote/repo

set +x
echo creating config/private.yml with blobstore secrets
cat > config/private.yml << EOF
---
blobstore:
  provider: s3
  options:
    access_key_id: $aws_access_key_id
    secret_access_key: $aws_secret_access_key
EOF
set -x

echo "finalizing CPI release..."
bosh finalize-release $DEV_RELEASE/*.tgz \
  --version $version

bosh create-release releases/bosh-kubernetes-cpi/bosh-kubernetes-cpi-${version}.yml
  --tarball final-release/bosh-kubernetes-cpi-$version.tgz

rm config/private.yml

git diff | cat
git add .

git config --global user.email jan.von.loewenstein@sap.com
git config --global user.name "Jan von Loewenstein"
git commit -m "New final release v$version"
