{ units, define, ... }:

{
  unit = define.group {
    config = [
      units.agents.skills.codex-review.unit
      units.agents.skills.notify.unit
      units.agents.skills.nushell-development.unit
      units.agents.skills.pull-requests.unit
      units.agents.skills.using-nix.unit
    ];
  };
}
