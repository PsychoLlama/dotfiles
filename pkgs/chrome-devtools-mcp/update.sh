#!/usr/bin/env bash
set -euo pipefail

# nix-update resolves the flake from the working directory, so run from the repo root.
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'
