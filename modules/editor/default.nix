{ config, lib, pkgs, ... }:

with lib;

let
  vimrc = pkgs.writeText "user-config.vim" config.extraConfig;

  # This is the generated `&packpath` directory for all plugins.
  packdir = pkgs.vimUtils.packDir config.neovim.packpathDirs;

  # Name of the package bundle added in `&packpath`. As of this writing, the
  # Neovim wrapper calls it "myNeovimPackages".
  bundleName = elemAt (attrNames config.neovim.packpathDirs) 0;

  # The `opt/` directory contains a list of symlinks: {pluginName} -> {plugin}
  # Those plugin names are what we use to load them: `:packadd {pluginName}`
  bundlePath = "${packdir}/pack/${bundleName}/opt";

  # Packages are loaded with `opt/` instead of `start/` so we can dynamically
  # disable them according to configuration/environment. This has side
  # effects. The most notable is that `start/` packages are implicitly
  # available like `&runtimepath`, whereas lazy packages are only added after
  # calling `:packadd`.
  #
  # This poses a challenge because packages must load after the vimrc, but the
  # vimrc may need them ahead of time (e.g. requiring a lua plugin).
  #
  # As a workaround, we temporarily add lazy packages to the `&runtimepath`,
  # source the vimrc, then reset it and load them normally.
  startupFile = pkgs.writeText "package-loader.lua" ''
    local original_rtp = vim.opt.rtp:get()
    local plugin_dir = ${generators.toLua { } bundlePath}

    for plugin_name in vim.fs.dir(plugin_dir) do
      local plugin_path = plugin_dir .. '/' .. plugin_name
      local after = plugin_path .. '/after'

      vim.opt.rtp:prepend(plugin_path)

      if vim.fn.isdirectory(after) == 1 then
        vim.opt.rtp:append(after)
      end
    end

    vim.cmd.source(${generators.toLua { } vimrc.outPath})

    vim.opt.rtp = original_rtp
    for plugin_name in vim.fs.dir(plugin_dir) do
      vim.cmd.packadd(plugin_name)
    end
  '';

in {
  imports = [ ./plugins.nix ./presets ];

  options = {
    enable = mkEnableOption "Whether to enable Neovim";

    neovim = mkOption {
      type = types.package;
      description = "The generated neovim package";
      readOnly = true;
      default = config.package.override {
        inherit (config) withNodeJs;

        extraMakeWrapperArgs =
          "--suffix PATH : ${lib.makeBinPath config.extraPackages}";

        configure = {
          packages.plugins.opt = config.extraPlugins;
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
