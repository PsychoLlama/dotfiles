#!/usr/bin/env bash
set -euo pipefail

manifest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/manifest.json"
repo=$(jq -r .repo "$manifest")
frontend_repo=$(jq -r .devtoolsFrontend.repo "$manifest")

tag=$(gh release view --repo "$repo" --json tagName --jq .tagName)
version="${tag#chrome-devtools-mcp-v}"

# Resolves the SRI hash of a GitHub source tree.
prefetch() {
  nix flake prefetch --json --refresh "github:$1/$2" | jq -er .hash
}

hash=$(prefetch "$repo" "$tag")

# Upstream vendors devtools-frontend as a submodule, but it recursively depends
# on a private chrome-internal repo. Pin the submodule rev and fetch it alone.
frontend_rev=$(gh api "repos/$repo/contents/third_party/devtools-frontend?ref=$tag" --jq .sha)
frontend_hash=$(prefetch "$frontend_repo" "$frontend_rev")

tmp=$(mktemp)
jq --arg version "$version" --arg hash "$hash" \
  --arg frontendRev "$frontend_rev" --arg frontendHash "$frontend_hash" \
  '.version = $version
   | .hash = $hash
   | .devtoolsFrontend.rev = $frontendRev
   | .devtoolsFrontend.hash = $frontendHash' "$manifest" > "$tmp"

mv "$tmp" "$manifest"

# npm dependencies change with the lockfile, so resolve the hash from a build.
npm_deps_hash=$(
  nix build --no-link ".#chrome-devtools-mcp.npmDeps" 2>&1 \
    | awk '/got: /{ print $2 }'
)

if [[ -n "$npm_deps_hash" ]]; then
  tmp=$(mktemp)
  jq --arg hash "$npm_deps_hash" '.npmDepsHash = $hash' "$manifest" > "$tmp"
  mv "$tmp" "$manifest"
fi

echo "Updated manifest.json to $version"
