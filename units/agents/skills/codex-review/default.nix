{ units, module, ... }:

{
  unit = module.component {
    config = [
      (units.agents.skill {
        name = "codex-review";
        targets = [ "claude" ];
        description = ''Run `codex review` and surface only the final review. Use whenever asked to "ask codex to review", "get a codex review", or otherwise invoke codex's code review subagent. Captures full transcript to a tempfile so failures stay debuggable.'';
        body = ./SKILL.md;
      })
    ];
  };
}
