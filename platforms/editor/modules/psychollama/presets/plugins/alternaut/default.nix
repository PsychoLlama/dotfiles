{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.alternaut-nvim;
in

{
  config.plugins.alternaut-nvim = lib.mkIf cfg.enable {
    opts = rec {
      python = {
        file_naming_conventions = [
          "test_{name}.{ext}"
          "{name}.{ext}"
        ];
        directory_naming_conventions = [ "tests" ];
        file_extensions = [ "py" ];
      };

      lua = {
        file_naming_conventions = [ "{name}_spec.{ext}" ];
        directory_naming_conventions = [ "." ];
        file_extensions = [ "lua" ];
      };

      "javascript.jsx" = javascript;
      javascript = {
        file_naming_conventions = [ "{name}.test.{ext}" ];
        directory_naming_conventions = [ "__tests__" ];
        file_extensions = [ "js" ];
      };

      "typescript.tsx" = typescript;
      "typescriptreact" = typescript;
      typescript = {
        file_naming_conventions = [
          "{name}.test.{ext}"
          "{name}.unit.{ext}"
        ];
        directory_naming_conventions = [ "__tests__" ];
        file_extensions = [
          "ts"
          "tsx"
          "js"
          "jsx"
        ];
      };

      vader = vim;
      vim = {
        file_naming_conventions = [ "{name}.{ext}" ];
        directory_naming_conventions = [ "tests" ];
        file_extensions = [
          "vim"
          "vader"
        ];
      };
    };
  };
}
