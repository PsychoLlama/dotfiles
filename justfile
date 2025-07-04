set shell := ["nu", "-c"]

_:
  just --list

activate mode="test":
  sudo true # Make sure we've got sudo access
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . err+out>| nom --json
