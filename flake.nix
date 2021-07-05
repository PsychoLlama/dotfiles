{
  description = "NixOS modules supporting my development environment";

  inputs.vim-plugin-nursery.url = "github:PsychoLlama/vim-plugin-nursery/main";
  inputs.nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

  outputs = inputs:
  let lib = import ./lib.nix inputs;

  in {
    nixosModule = import ./default.nix;
    overlay = import ./pkgs;

    nixosConfigurations = {
      ava = lib.defineHost "x86_64-linux" ./hosts/ava;
    };
  };
}
