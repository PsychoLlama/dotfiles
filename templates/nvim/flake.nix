{
  description = "Development environment";

  inputs = {
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
    }:

    let
      inherit (nixpkgs) lib;

      eachSystem = lib.flip lib.mapAttrs (
        lib.genAttrs (import systems) (system: import nixpkgs { inherit system; })
      );
    in

    {
      devShells = eachSystem (
        system: pkgs:
        let
          default = pkgs.mkShell {
            packages = [
              pkgs.just
              pkgs.lua-language-server
              pkgs.luajitPackages.luacheck
              pkgs.luajitPackages.vusted
              pkgs.nixfmt
              pkgs.prettier
              pkgs.stylua
              pkgs.treefmt
            ];
          };
        in
        {
          inherit default;

          ci = pkgs.mkShell {
            inputsFrom = [ default ];
            packages = [ pkgs.neovim ];
          };
        }
      );
    };
}
