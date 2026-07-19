#!/usr/bin/env bash
set -euo pipefail

manifest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/manifest.json"
repo=$(jq -r .repo "$manifest")

release=$(gh release view --repo "$repo" --json tagName,assets)
version=$(jq -er '.tagName | ltrimstr("rust-v")' <<<"$release")

platforms=$(jq -n '{}')
for platform in \
  x86_64-unknown-linux-musl \
  aarch64-unknown-linux-musl \
  x86_64-apple-darwin \
  aarch64-apple-darwin; do
  asset="codex-$platform.tar.gz"
  digest=$(jq -er --arg asset "$asset" \
    '.assets[] | select(.name == $asset) | .digest' <<<"$release") || {
    echo "error: latest $repo release ($version) has no digest for $asset" >&2
    exit 1
  }
  hash=$(nix hash convert --hash-algo sha256 --to sri "${digest#sha256:}")
  platforms=$(jq --arg p "$platform" --arg h "$hash" '.[$p] = $h' <<<"$platforms")
done

tmp=$(mktemp)
jq --arg version "$version" --argjson platforms "$platforms" \
  '.version = $version | .platforms = $platforms' "$manifest" > "$tmp"

mv "$tmp" "$manifest"
echo "Updated manifest.json to $version"
