{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.vue;
  vueLanguageServerPath = "${cfg.package}/lib/language-tools/packages/language-server";
in

{
  options.psychollama.presets.lsp.servers.vue = {
    enable = lib.mkEnableOption "Use Vue language server";
    package = lib.mkPackageOption pkgs.unstable "vue-language-server" { };
  };

  config = lib.mkIf cfg.enable {
    # Vue language server handles CSS/HTML in .vue files (hybrid mode).
    lsp.servers.vue = {
      server = "${cfg.package}/bin/vue-language-server";
      args = [ "--stdio" ];
      filetypes = [ "vue" ];

      root.patterns = [
        "vite.config.ts"
        "vite.config.js"
        "vue.config.js"
        "nuxt.config.ts"
        "nuxt.config.js"
        "package.json"
        ".git/"
      ];
    };

    # vtsls handles TS/JS in .vue files via @vue/typescript-plugin.
    lsp.servers.vtsls = {
      filetypes = [ "vue" ];
      settings.vtsls.tsserver.globalPlugins = [
        {
          name = "@vue/typescript-plugin";
          location = vueLanguageServerPath;
          languages = [ "vue" ];
          configNamespace = "typescript";
        }
      ];
    };

    # Bridge handler to forward tsserver requests from Vue LSP to vtsls.
    extraConfig = builtins.readFile ./vue/bridge.lua;
  };
}
