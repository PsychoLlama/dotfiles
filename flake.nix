{
  description = "NixOS modules supporting my development environment";

  inputs.vim-plugin-nursery.src = "github:PsychoLlama/vim-plugin-nursery/main";
  inputs.nixpkgs-unstable.src = "github:NixOS/nixpkgs/master";

  outputs = inputs: {
    nixosModules = {
      dotfiles = nixos: import ./default.nix (nixos // inputs);
    };
  };
}
