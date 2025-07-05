set shell := ["nu", "-c"]

_:
  just --list

activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . err+out>| nom --json
