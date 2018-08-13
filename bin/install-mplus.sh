#!/bin/bash

# This script installs the M+ font which is my preferred font
# to use Emacs with. Since emacs fails if it tries to use a font
# that doesn't exist this is often one of the first things I do
# on a new desktop system.
#
# Note that the script copies files to /Library/Fonts so it may
# not be suitable to non-Mac systems.

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
