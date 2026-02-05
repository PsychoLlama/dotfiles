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
  gcc,
  ripgrep,
}:
let
  stdenv = stdenvNoCC;
  manifest = lib.importJSON ./manifest.json;
  platformKey =
    {
      "x86_64-linux" = "x86_64-unknown-linux-gnu";
      "aarch64-linux" = "aarch64-unknown-linux-gnu";
      "x86_64-darwin" = "x86_64-apple-darwin";
      "aarch64-darwin" = "aarch64-apple-darwin";
    }
    .${stdenv.hostPlatform.system};
in
stdenv.mkDerivation (finalAttrs: {
  pname = "codex";
  inherit (manifest) version;

  src = fetchurl {
    url = "https://github.com/openai/codex/releases/download/rust-v${finalAttrs.version}/codex-${platformKey}.tar.gz";
    hash = manifest.platforms.${platformKey};
  };

  sourceRoot = ".";
  dontBuild = true;
  dontStrip = true;

  nativeBuildInputs =
    [ makeBinaryWrapper ]
    ++ lib.optionals stdenv.hostPlatform.isElf [ autoPatchelfHook ];

  buildInputs = lib.optionals stdenv.hostPlatform.isElf [
    libcap
    openssl
    gcc.cc.lib
  ];

  strictDeps = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 codex-${platformKey} $out/bin/codex
    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/codex --prefix PATH : ${lib.makeBinPath [ ripgrep ]}
  '';

  meta = {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/openai/codex";
    changelog = "https://raw.githubusercontent.com/openai/codex/refs/tags/rust-v${finalAttrs.version}/CHANGELOG.md";
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
