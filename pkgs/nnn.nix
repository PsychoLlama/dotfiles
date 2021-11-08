{ runCommand, makeWrapper, nnn, }:

let
  # I refuse to use their plugin download script. This is a land of purity.
  plugins = runCommand "prepare-nnn-plugins" { inherit (nnn) src; } ''
    mkdir -p "$out/nnn"
    cp -r "$src/plugins" "$out/nnn"

    # If these directories aren't created ahead of time, nnn will fail
    # trying to initialize them on a read-only file system. Silly nnn.
    mkdir "$out"/nnn/{sessions,mounts,bookmarks}
  '';

in nnn.overrideAttrs (attrs: {
  useNerdFonts = true;
  nativeBuildInputs = attrs.nativeBuildInputs ++ [ makeWrapper ];
  postInstall = attrs.postInstall + ''
    wrapProgram $out/bin/nnn --add-flags -R \
      --set XDG_CONFIG_HOME "${plugins}" \
      --set NNN_FIFO /tmp/nnn.fifo \
      --set NNN_PLUG 'z:autojump'
  '';
})
