flake-inputs:

flake-inputs.nixpkgs.lib.extend (
  self: super: {
    dotfiles = {
      buildEditor = import ./build-editor.nix flake-inputs;
    };
  }
)
