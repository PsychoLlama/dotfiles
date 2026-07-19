#!/usr/bin/env bash
set -euo pipefail

manifest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/manifest.json"
repo=$(jq -r .repo "$manifest")

release=$(gh release view --repo "$repo" --json tagName,assets)
version=$(jq -er '.tagName' <<<"$release")

asset="nvim-$version-x86_64-linux.tar.gz"
digest=$(jq -er --arg asset "$asset" \
  '.assets[] | select(.name == $asset) | .digest' <<<"$release") || {
  echo "error: latest $repo release ($version) has no digest for $asset" >&2
  exit 1
}

hash=$(nix hash convert --hash-algo sha256 --to sri "${digest#sha256:}")

tmp=$(mktemp)
jq --arg version "$version" --arg hash "$hash" \
  '.version = $version | .hash = $hash' "$manifest" > "$tmp"

mv "$tmp" "$manifest"
echo "Updated manifest.json to $version"
