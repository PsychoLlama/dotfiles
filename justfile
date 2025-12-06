set shell := ["nu", "-c"]

_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . err+out>| nom --json

# Format all files.
fmt:
  treefmt

# Run luacheck on all Lua files.
lint:
  luacheck pkgs platforms

# Run Lua tests with vusted.
test:
  vusted pkgs platforms

# Run all checks (lint + test).
check: lint test
