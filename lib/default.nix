flake-inputs:

{
  defineHost = import ./define-host.nix flake-inputs;
  buildEditor = import ./build-editor.nix flake-inputs;
}
