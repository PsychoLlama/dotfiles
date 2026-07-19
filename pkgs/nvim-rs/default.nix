# My custom fork of neovim: https://github.com/PsychoLlama/nvim.rs
{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  neovim-unwrapped,
}:

let
  inherit (lib.importJSON ./manifest.json) repo version hash;
in

stdenv.mkDerivation (finalAttrs: {
  pname = "nvim-rs";
  inherit version;

  src = fetchurl {
    url = "https://github.com/${repo}/releases/download/${version}/nvim-${version}-x86_64-linux.tar.gz";
    inherit hash;
  };

  # Upstream binary is built for linux targets, not NixOS specifically.
  nativeBuildInputs = [ autoPatchelfHook ];

  # The binary and parsers link libc/libm (glibc, implicit) and libgcc_s.
  buildInputs = [ stdenv.cc.cc.lib ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r bin lib share $out/

    # `wrapNeovim` unconditionally rewrites this desktop entry on Linux, and the
    # upstream tarball doesn't ship one. Borrow the stock Neovim entry.
    install -Dm644 ${neovim-unwrapped}/share/applications/nvim.desktop \
      $out/share/applications/nvim.desktop

    runHook postInstall
  '';

  passthru = {
    # `wrapNeovim` reads `.lua` to build the plugin Lua environment. The c2rust
    # build embeds the same LuaJIT host as upstream Neovim, so reuse it.
    inherit (neovim-unwrapped) lua;
  };

  meta = neovim-unwrapped.meta // {
    description = "Rust rewrite of neovim";
    homepage = "https://github.com/${repo}";
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];

    # Only compiled for Linux, sadly. `c2rust` baked in some target assumptions.
    platforms = [ "x86_64-linux" ];
  };
})
