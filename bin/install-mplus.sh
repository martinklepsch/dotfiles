#!/bin/bash

set -eou pipefail

version="mplus-TESTFLIGHT-063"
dir=$(mktemp -d)

echo "Downloading..."
curl \
  -sL "https://osdn.net/dl/mplus-fonts/$version.tar.xz" \
  -o "$dir/mplus.tar.xz"

echo "Unpacking..."
tar -xf "$dir/mplus.tar.xz" -C "$dir"

echo "Installing..."
cp $dir/$version/mplus*.ttf "/Library/Fonts"

echo "Cleanup..."
rm -rf "$dir"
