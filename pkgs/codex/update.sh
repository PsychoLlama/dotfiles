#!/usr/bin/env nix
#!nix shell --ignore-environment nixpkgs#cacert nixpkgs#coreutils nixpkgs#curl nixpkgs#bash nixpkgs#jq nixpkgs#nix --command bash

set -euo pipefail

REPO="openai/codex"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get the latest release tag matching rust-v*
VERSION=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | jq -r '.tag_name | ltrimstr("rust-v")')

echo "Latest version: $VERSION"

platforms=("x86_64-unknown-linux-gnu" "aarch64-unknown-linux-gnu" "x86_64-apple-darwin" "aarch64-apple-darwin")

manifest=$(jq -n --arg version "$VERSION" '{ version: $version, platforms: {} }')

for platform in "${platforms[@]}"; do
  url="https://github.com/$REPO/releases/download/rust-v$VERSION/codex-$platform.tar.gz"
  echo "Fetching hash for $platform..."
  hash=$(nix hash convert --hash-algo sha256 $(nix-prefetch-url "$url" 2>/dev/null))
  manifest=$(echo "$manifest" | jq --arg p "$platform" --arg h "$hash" '.platforms[$p] = $h')
done

echo "$manifest" | jq . > "$SCRIPT_DIR/manifest.json"
echo "Updated manifest.json to version $VERSION"
