with (import <nixpkgs> {});

with (rec {
  importPlugin = input:
    let plugin = normalizePlugin input; in
    if isDerivation (builtins.elemAt plugin 0) then plugin else
    [(fetchFromGithub (builtins.elemAt plugin 0)) (builtins.elemAt plugin 1)];

  fetchFromGithub = path:
    let parts = parseRepoPath path; in fetchGit {
      url = "https://github.com/${parts.owner}/${parts.repo}.git";
      name = "${parts.owner}--${parts.repo}";
    };

  normalizePlugin = plugin:
    if builtins.isList plugin then plugin
    else [plugin {}];

  isDerivation = value:
    builtins.isAttrs value && value.type == "derivation";

  parseRepoPath = path:
    let parts = builtins.filter builtins.isString (builtins.split "/" path); in
    if validRepoPath parts then
      { owner = builtins.elemAt parts 0; repo = builtins.elemAt parts 1; }
    else
      builtins.throw "Not a valid repo path (${path}).";

  validRepoPath = parts:
    builtins.length parts == 2 &&
    builtins.map builtins.isString parts == [true true];
});

{
  manifest = plugins: derivation rec {
    name = "vim-manifest";
    builder = "${bash}/bin/bash";
    system = builtins.currentSystem;
    args = [ ./builder.bash ];
    manifest = builtins.toJSON {
      plugins = builtins.map importPlugin plugins;
    };
  };
}
