{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "agents.skills.using-nix" {
  unit = rhizome.component [
    (home-manager.module ({
      agents.skills.using-nix = {
        description = "ALWAYS use this skill for anything related to Nix, NixOS, home-manager, or nixpkgs. This includes writing Nix code, looking up modules/options/packages, answering questions, and debugging Nix errors.";
        template.body = ./SKILL.md;
      };
    }))
  ];
}
