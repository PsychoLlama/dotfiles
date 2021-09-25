{
  inputs.dotfiles.url = "github:PsychoLlama/dotfiles/main";
  inputs.nixpkgs.url = "nixpkgs/nixos-21.05";

  outputs = { self, dotfiles, nixpkgs }: {
    nixosConfigurations = {
      personal-computer = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        specialArgs = { inherit self; };

        modules = [ dotfiles.nixosModules.default ./configuration.nix ];
      };
    };
  };
}
