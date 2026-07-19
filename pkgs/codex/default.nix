# Forked from nixpkgs to avoid waiting on merge delays for new releases.
# Uses prebuilt binaries from GitHub releases instead of building from source.
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/co/codex/package.nix
{
  lib,
  stdenvNoCC,
  fetchurl,
  makeBinaryWrapper,
  autoPatchelfHook,
  libcap,
  openssl,
  zlib,
  gcc,
  ripgrep,
  bubblewrap,
}:
let
  stdenv = stdenvNoCC;
  manifest = lib.importJSON ./manifest.json;
  inherit (manifest) repo;
  inherit (manifest.platforms.${stdenv.hostPlatform.system}) target hash;
in
stdenv.mkDerivation (finalAttrs: {
  pname = "codex-bin";
  inherit (manifest) version;

  src = fetchurl {
    url = "https://github.com/${repo}/releases/download/rust-v${finalAttrs.version}/codex-${target}.tar.gz";
    inherit hash;
  };

  sourceRoot = ".";
  dontBuild = true;
  dontStrip = true;

  nativeBuildInputs = [
    makeBinaryWrapper
  ]
  ++ lib.optionals stdenv.hostPlatform.isElf [ autoPatchelfHook ];

  buildInputs = lib.optionals stdenv.hostPlatform.isElf [
    libcap
    openssl
    zlib
    gcc.cc.lib
  ];

  strictDeps = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 codex-${target} $out/bin/codex
    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/codex --prefix PATH : ${
      lib.makeBinPath ([ ripgrep ] ++ lib.optionals stdenv.hostPlatform.isLinux [ bubblewrap ])
    }
  '';

  meta = {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/${repo}";
    changelog = "https://raw.githubusercontent.com/${repo}/refs/tags/rust-v${finalAttrs.version}/CHANGELOG.md";
    license = lib.licenses.asl20;
    mainProgram = "codex";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  };
})
