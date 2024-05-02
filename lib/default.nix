flake-inputs:

flake-inputs.nixpkgs.lib.extend (
  self: super: {
    dotfiles = {
      defineHost = import ./define-host.nix flake-inputs;
      buildEditor = import ./build-editor.nix flake-inputs;
    };
  }
)
