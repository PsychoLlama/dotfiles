{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component "unit" {
    config = [
      (home-manager.lib.program "fd")

      (home-manager.module ({
        # `.git/` isn't covered by any .gitignore (git special-cases it), so
        # `fd --hidden` would otherwise descend the entire internal object tree.
        # The global ignore file keeps it out of every git-visible listing.
        programs.fd.ignores = [ ".git/" ];
        programs.fzf.defaultCommand = "fd --hidden --type f";
      }))
    ];
  };
}
