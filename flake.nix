{
  description = "NixOS modules supporting my development environment";

  inputs.vim-plugin-nursery.url = "github:PsychoLlama/vim-plugin-nursery/main";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";

  outputs = inputs:
  let lib = import ./lib.nix inputs;

  in {
    nixosModule = lib.defineModule ./default.nix;
    overlay = import ./pkgs;

    nixosConfigurations = {
      ava = lib.defineHost ./hosts/ava;
    };
  };
}
