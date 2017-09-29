#!/bin/bash
set -euo pipefail

GO_VERSION="1.9"

export GoInstallDir="/opt/go"
mkdir -p $GoInstallDir

if [ ! -f $GoInstallDir/go/bin/go ]; then
  GO_MD5="4577d9ba083ac86de78012c04a2981be"
  URL=https://buildpacks.cloudfoundry.org/dependencies/go/go${GO_VERSION}.linux-amd64-${GO_MD5:0:8}.tar.gz

  echo "-----> Download go ${GO_VERSION}"
  curl -s -L --retry 15 --retry-delay 2 $URL -o /tmp/go.tar.gz

  DOWNLOAD_MD5=$(md5sum /tmp/go.tar.gz | cut -d ' ' -f 1)

  if [[ $DOWNLOAD_MD5 != $GO_MD5 ]]; then
    echo "       **ERROR** MD5 mismatch: got $DOWNLOAD_MD5 expected $GO_MD5"
    exit 1
  fi

  tar xzf /tmp/go.tar.gz -C $GoInstallDir
  rm /tmp/go.tar.gz
fi
if [ ! -f $GoInstallDir/go/bin/go ]; then
  echo "       **ERROR** Could not download go"
  exit 1
fi

cd $GoInstallDir
mv go ${GO_VERSION}
ln -s ${GO_VERSION} current
rm sources.yml
cd

