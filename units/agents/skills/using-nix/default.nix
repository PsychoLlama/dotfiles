{ rhizome, units, ... }:

rhizome.unit "agents.skills.using-nix" {
  unit = rhizome.component [
    (units.agents.skill {
      name = "using-nix";
      description = "ALWAYS use this skill for anything related to Nix, NixOS, home-manager, or nixpkgs. This includes writing Nix code, looking up modules/options/packages, answering questions, and debugging Nix errors.";
      body = ./SKILL.md;
    })
  ];
}
