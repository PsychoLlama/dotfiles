{ lib, pkgs, ... }:

{
  platforms.home-manager.programs = {
    fd = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.fd;

      # `.git/` isn't covered by any .gitignore (git special-cases it), so
      # `fd --hidden` would otherwise descend the entire internal object tree.
      # The global ignore file keeps it out of every git-visible listing.
      ignores = [ ".git/" ];
    };

    fzf.defaultCommand = "fd --hidden --type f";
  };
}
