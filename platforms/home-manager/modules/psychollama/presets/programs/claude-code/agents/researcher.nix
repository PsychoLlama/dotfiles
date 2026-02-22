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

      tools = [
        "WebFetch"
        "WebSearch"
      ];
    };
  };
}
