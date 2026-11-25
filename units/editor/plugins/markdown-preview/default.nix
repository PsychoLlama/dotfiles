{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.plugins.markdown-preview" [
  # markdown-preview-nvim runs a Node.js server to render the preview.
  (editor.module ({ withNodeJs = true; }))
]
