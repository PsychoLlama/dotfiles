{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  vimrc = pkgs.writeText "user-config.vim" config.extraConfig;

  # This is the generated `&packpath` directory for all plugins.
  packdir = pkgs.vimUtils.packDir { managed-by-nix.opt = config.extraPlugins; };

  # The `opt/` directory contains a list of symlinks: {pluginName} -> {plugin}
  # Those plugin names are what we use to load them: `:packadd {pluginName}`
  bundlePath = "${packdir}/pack/managed-by-nix/opt";

  # Packages are loaded with `opt/` instead of `start/` so we can dynamically
  # disable them according to configuration/environment. This has side
  # effects. The most notable is that `start/` packages are implicitly
  # available like `&runtimepath`, whereas lazy packages are only added after
  # calling `:packadd`.
  #
  # This poses a challenge because packages must load after the vimrc, but the
  # vimrc may need them ahead of time (e.g. requiring a lua plugin).
  #
  # As a workaround, we add lazy packages to the `&runtimepath`, then source
  # the vimrc, then load them correctly.
  startupFile = pkgs.writeText "package-loader.lua" ''
    local plugin_dir = ${generators.toLua { } bundlePath}

    -- Create a mapping of all dynamic plugins:
    -- Format: "example-nvim:/path/to/example;another:/path/to/another"
    local dynamic_plugins = {}
    if vim.env.VIM_PLUGINS then
      table.foreach(vim.split(vim.env.VIM_PLUGINS, ';'), function(_, mapping)
        local plugin_name, plugin_path = unpack(vim.split(mapping, ':'))
        dynamic_plugins[plugin_name] = vim.fs.normalize(plugin_path)
      end)
    end

    -- Create a mapping of all static plugins:
    -- { "plugin.vim" = "/plugin/path" }
    local static_plugins = {}
    for plugin_name in vim.fs.dir(plugin_dir) do
      if not dynamic_plugins[plugin_name] then
        local plugin_path = plugin_dir .. '/' .. plugin_name
        static_plugins[plugin_name] = plugin_path
      end
    end

    local function export_plugin(plugin_name, plugin_path)
      vim.opt.rtp:prepend(plugin_path)

      local after = plugin_path .. '/after'
      if vim.fn.isdirectory(after) == 1 then
        vim.opt.rtp:append(after)
      end
    end

    table.foreach(dynamic_plugins, export_plugin)
    table.foreach(static_plugins, export_plugin)

    vim.cmd.source(${generators.toLua { } vimrc.outPath})

    table.foreach(static_plugins, function(plugin_name)
      vim.cmd.packadd(plugin_name)
    end)
  '';
in
{
  imports = [
    ./plugins.nix
    ./presets
  ];

  options = {
    enable = mkEnableOption "Whether to enable Neovim";

    neovim = mkOption {
      type = types.package;
      description = "The generated neovim package";
      readOnly = true;
      default = config.package.override {
        inherit (config) withNodeJs;

        extraMakeWrapperArgs = concatStringsSep " " [
          "--suffix PATH : ${lib.makeBinPath config.extraPackages}"
          (lib.cli.toGNUCommandLineShell { } {
            add-flags = [
              ''--cmd "set packpath^=${packdir}"''
              ''--cmd "set rtp^=${packdir}"''
            ];
          })
        ];

        configure = {
          # packages.plugins.opt = config.extraPlugins;
          customRC = "source ${startupFile}";
        };
      };
    };

    package = mkOption {
      type = types.package;
      description = "A neovim editor";
      default = pkgs.neovim;
    };

    withNodeJs = mkOption {
      type = types.bool;
      description = "Whether to enable Node.js support";
      default = true;
    };

    extraPlugins = mkOption {
      type = types.listOf types.package;
      description = "Extra plugins to install";
      default = [ ];
    };

    extraPackages = mkOption {
      type = types.listOf types.package;
      description = "Extra packages visible to Neovim";
      default = [ ];
    };

    extraConfig = mkOption {
      type = types.lines;
      description = "Extra vimrc config";
      default = "";
    };
  };
}
