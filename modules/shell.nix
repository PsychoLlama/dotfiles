{ inputs, ... }:

{
  perSystem =
    { pkgs, system, ... }:
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.agenix.packages.${system}.default
          pkgs.unstable.just
          pkgs.unstable.lua-language-server
          pkgs.unstable.luajitPackages.luacheck
          pkgs.unstable.luajitPackages.vusted
          pkgs.unstable.nh
          pkgs.unstable.nix-update
          pkgs.unstable.nixfmt
          pkgs.unstable.prettier
          pkgs.unstable.stylua
          pkgs.unstable.treefmt
        ];
      };
    };
}
