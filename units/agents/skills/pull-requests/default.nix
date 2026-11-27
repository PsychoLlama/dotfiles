{ units, define, ... }:

{
  unit = define.unit {
    config = [
      (units.agents.skill {
        name = "pull-requests";
        description = ''Open, iterate on, and merge PRs for the current repo. Load when asked to open a PR (e.g. "PR when done", "lmk when the PR's ready", "Commit + PR"), push to an open PR, or merge one.'';
        body = ./SKILL.md;
      })
    ];
  };
}
