# -> flake-inputs
{
  self,
  home-manager,
  ...
}:

# -> callPackage
{
  lib,
  pkgs,
  modules,
  prefix,
  platform ? null,
}:

let
  revision = self.rev or "main";
  homepage = "https://github.com/PsychoLlama/dotfiles/tree/${revision}";

  # Clean module environment - no NixOS options available.
  pristine = lib.evalModules {
    specialArgs = { inherit pkgs lib; };
    inherit modules;
  };

  # Evaluate modules in a minimal NixOS environment. This is lighter than
  # creating a new machine.
  nixos = import (pkgs.path + "/nixos/lib/eval-config.nix") {
    system = pkgs.stdenv.hostPlatform.system;
    inherit pkgs;
    inherit modules;
  };

  allOptions = lib.optionAttrSetToDocList (
    if platform == "nixos" then nixos.options else pristine.options
  );

  # Only generate documentation for things under the `psychollama.*` namespace.
  filteredOptions = lib.filter (
    opt: opt.visible && !opt.internal && (lib.hasPrefix prefix opt.name)
  ) allOptions;

  # Convert to the format expected by `nixos-render-docs`.
  optionSet = lib.listToAttrs (
    lib.flip map filteredOptions (opt: {
      inherit (opt) name;
      value = lib.attrsets.removeAttrs opt [ "name" ];
    })
  );

  # Write the options file WITHOUT compiling mentioned dependencies. The
  # documentation generator will not traverse into listed store paths.
  optionsJSON = builtins.toFile "options.json" (
    builtins.unsafeDiscardStringContext (builtins.toJSON optionSet)
  );
in

pkgs.runCommand "generate-markdown-docs"
  {
    nativeBuildInputs = [ pkgs.nixos-render-docs ];
    allowedReferences = [
      "${self.outPath}"
      "out"
    ];
  }
  ''
    mkdir $out
    nixos-render-docs -j $NIX_BUILD_CORES options commonmark \
    --revision ${lib.escapeShellArg revision} \
    --manpage-urls ${pkgs.path + "/doc/manpage-urls.json"} \
    ${optionsJSON} \
    $out/options.md

    # Replace links to point to GitHub instead of the file system.
    sed -i "$out/options.md" \
    -e 's_file://${self.outPath}_${homepage}_g' "$out/options.md" \
    -e 's_${self.outPath}/__g' "$out/options.md"
  ''
