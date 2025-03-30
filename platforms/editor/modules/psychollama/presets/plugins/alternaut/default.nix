{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.alternaut-nvim;
in

{
  config.plugins.alternaut-nvim = lib.mkIf cfg.enable {
    opts = {
      modes = {
        test = {
          vitest = {
            patterns = [
              "{name}.test.{ext}"
              "{name}.unit.{ext}"
            ];
            directories = [ "__tests__" ];
            extensions = [
              "ts"
              "tsx"
              "js"
              "jsx"
            ];
          };

          go = {
            patterns = [ "{name}_spec.{ext}" ];
            directories = [ "." ];
            extensions = [ "go" ];
          };

          busted = {
            patterns = [ "{name}_spec.{ext}" ];
            extensions = [ "lua" ];
            directories = [
              "."
              "spec"
            ];
          };

          pytest = {
            patterns = [
              "test_{name}.{ext}"
              "{name}_test.{ext}"
            ];
            extensions = [ "py" ];
            directories = [
              "tests"
              "."
            ];
          };

          vader = {
            patterns = [ "{name}.{ext}" ];
            extensions = {
              target = [ "vader" ];
              origin = [ "vim" ];
            };
            directories = [
              "tests"
              "."
            ];
          };
        };

        style = {
          vanilla_extract = {
            patterns = [ "{name}.css.{ext}" ];
            extensions = {
              target = [ "ts" ];
              origin = [
                "tsx"
                "ts"
              ];
            };
          };

          css = {
            patterns = [ "{name}.{ext}" ];
            extensions = {
              target = [
                "css"
                "less"
              ];
              origin = [
                "tsx"
                "jsx"
                "ts"
                "js"
              ];
            };
          };
        };

        template = {
          vue = {
            patterns = [ "{name}.vue" ];
            extensions = {
              target = [ "vue" ];
              origin = [
                "ts"
                "js"
              ];
            };
          };
        };

        header = {
          c = {
            patterns = [ "{name}.{ext}" ];
            extensions = {
              target = [
                "h"
                "hpp"
                "hh"
              ];
              origin = [
                "c"
                "cpp"
                "cc"
                "m"
                "mm"
              ];
            };
          };
        };
      };
    };
  };
}
