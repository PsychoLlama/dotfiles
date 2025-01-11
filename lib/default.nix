flake-inputs:

flake-inputs.nixpkgs.lib.extend (
  self: super: {
    dotfiles = {
      hosts = import ./hosts.nix flake-inputs;
      buildEditor = import ./build-editor.nix flake-inputs;
      generateMarkdownDocs = import ./generate-markdown-docs.nix flake-inputs;
    };
  }
)
