_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . 2>&1 | nom --json

# Format all files.
fmt:
  treefmt

# Update all flake inputs and custom packages.
update:
  nix flake update
  @just update-packages

# Update all custom packages.
update-packages:
  nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'
  ./pkgs/codex/update.sh
  ./pkgs/claude-code-bin/update.sh
