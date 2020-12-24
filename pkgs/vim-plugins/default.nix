{ fetchFromGitHub, vimUtils }:

let pluginManifest =
  let lockfile = builtins.fromJSON (builtins.readFile ./lockfile.json);
  in lockfile.plugins;

  vimPluginFromDefinition = def: vimUtils.buildVimPluginFrom2Nix {
    pname = builtins.replaceStrings ["."] ["-"] def.repo;
    version = def.version;
    meta.homepage = def.homepage;

    # For now only GH is supported.
    src = fetchFromGitHub {
      owner = def.owner;
      repo = def.repo;
      rev = def.rev;
      sha256 = def.hash;
      fetchSubmodules = true;
    };
  };

in map vimPluginFromDefinition pluginManifest
