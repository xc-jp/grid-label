#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

echo "Removing previous build artifacts..."
rm result*
rm -rf bin

echo "Building grid-label..."
mkdir bin
nix-build -A grid-label-linux
cp result/bin/grid-label bin/grid-label
echo "Done!"
