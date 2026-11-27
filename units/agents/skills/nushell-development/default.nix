{ units, module, ... }:

{
  unit = module.component {
    config = [
      (units.agents.skill {
        name = "nushell-development";
        description = "Nushell reference. Use when writing, editing, or reviewing `.nu` files, authoring custom commands / modules, writing standalone `nu` scripts, or validating changes with `nu-check`.";
        body = ./SKILL.md;
      })
    ];
  };
}
