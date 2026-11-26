{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.unit "editor.plugins.markdown-preview" {
  unit = rhizome.component [
    # markdown-preview-nvim runs a Node.js server to render the preview.
    (editor.module ({ withNodeJs = true; }))
  ];
}
