#!/usr/bin/env bash
set -euo pipefail

manifest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/manifest.json"
repo=$(jq -r .repo "$manifest")

release=$(gh release view --repo "$repo" --json tagName,assets)
version=$(jq -er '.tagName | ltrimstr("rust-v")' <<<"$release")

# Upstream names release assets by Rust target triple. Translate here so the
# derivation can index `platforms` by Nix system directly.
declare -A targets=(
  [x86_64-linux]=x86_64-unknown-linux-musl
  [aarch64-linux]=aarch64-unknown-linux-musl
  [x86_64-darwin]=x86_64-apple-darwin
  [aarch64-darwin]=aarch64-apple-darwin
)

platforms=$(jq -n '{}')
for system in "${!targets[@]}"; do
  target="${targets[$system]}"
  asset="codex-$target.tar.gz"
  digest=$(jq -er --arg asset "$asset" \
    '.assets[] | select(.name == $asset) | .digest' <<<"$release") || {
    echo "error: latest $repo release ($version) has no digest for $asset" >&2
    exit 1
  }
  hash=$(nix hash convert --hash-algo sha256 --to sri "${digest#sha256:}")
  platforms=$(jq --arg s "$system" --arg t "$target" --arg h "$hash" \
    '.[$s] = { target: $t, hash: $h }' <<<"$platforms")
done

tmp=$(mktemp)
jq --arg version "$version" --argjson platforms "$platforms" \
  '.version = $version | .platforms = $platforms' "$manifest" > "$tmp"

mv "$tmp" "$manifest"
echo "Updated manifest.json to $version"
