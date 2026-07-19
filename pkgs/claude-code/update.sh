#!/usr/bin/env bash
set -euo pipefail

manifest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/manifest.json"
base_url=$(jq -r .baseUrl "$manifest")

version=$(curl -fsSL "$base_url/latest")
upstream=$(curl -fsSL "$base_url/$version/manifest.json")

tmp=$(mktemp)
jq --arg baseUrl "$base_url" '{
  baseUrl: $baseUrl,
  version,
  platforms: (.platforms | map_values(.checksum)),
}' <<<"$upstream" > "$tmp"

mv "$tmp" "$manifest"
echo "Updated manifest.json to $version"
