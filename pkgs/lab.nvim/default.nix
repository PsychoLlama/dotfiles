{ lib, vimUtils }:

vimUtils.buildVimPlugin {
  pname = "lab.nvim";
  version = "latest";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.difference ./. (
      lib.fileset.fileFilter (f: lib.hasSuffix "_spec.lua" f.name) ./.
    );
  };
}
