{ inputs, ... }:

{
  flake.lib = (import ../../lib inputs).dotfiles;
}
