{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "agents.skills.pull-requests" {
  unit = rhizome.component [
    (home-manager.module ({
      agents.skills.pull-requests = {
        description = ''Open, iterate on, and merge PRs for the current repo. Load when asked to open a PR (e.g. "PR when done", "lmk when the PR's ready", "Commit + PR"), push to an open PR, or merge one.'';
        template.body = ./SKILL.md;
      };
    }))
  ];
}
