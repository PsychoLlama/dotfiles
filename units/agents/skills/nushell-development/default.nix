{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "agents.skills.nushell-development" [
  (home-manager.module ({
    agents.skills.nushell-development = {
      description = "Nushell reference. Use when writing, editing, or reviewing `.nu` files, authoring custom commands / modules, writing standalone `nu` scripts, or validating changes with `nu-check`.";
      template.body = ./SKILL.md;
    };
  }))
]
