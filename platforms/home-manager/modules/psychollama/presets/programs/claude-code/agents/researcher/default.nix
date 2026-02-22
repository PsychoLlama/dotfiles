{
  config,
  lib,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
in

{
  config = lib.mkIf cfg.enable {
    programs.claude-code.agentManifest.researcher = {
      description = "Deep research agent with read-only tools.";
      prompt = builtins.readFile ./prompt.md;

      tools = [
        "Glob"
        "Grep"
        "Read"
        "WebFetch"
        "WebSearch"

        # Extended with read-only MCPs in downstream configs.
        "ListMcpResourcesTool"
        "ReadMcpResourcesTool"
      ];
    };
  };
}
