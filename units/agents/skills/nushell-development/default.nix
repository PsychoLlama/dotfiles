{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "agents.skills.nushell-development" {
  unit = rhizome.component [
    (home-manager.module ({
      agents.skills.nushell-development = {
        description = "Nushell reference. Use when writing, editing, or reviewing `.nu` files, authoring custom commands / modules, writing standalone `nu` scripts, or validating changes with `nu-check`.";
        template.body = ./SKILL.md;
      };
    }))
  ];
}
