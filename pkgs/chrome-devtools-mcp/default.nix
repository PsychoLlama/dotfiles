{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

let
  manifest = lib.importJSON ./manifest.json;

  # Vendored as a submodule upstream, but fetched separately: devtools-frontend
  # recursively depends on a private chrome-internal repo that Nix can't clone.
  devtools-frontend = fetchFromGitHub {
    owner = lib.head (lib.splitString "/" manifest.devtoolsFrontend.repo);
    repo = lib.last (lib.splitString "/" manifest.devtoolsFrontend.repo);
    inherit (manifest.devtoolsFrontend) rev hash;
  };
in

buildNpmPackage {
  pname = "chrome-devtools-mcp";
  inherit (manifest) version npmDepsHash;

  src = fetchFromGitHub {
    owner = lib.head (lib.splitString "/" manifest.repo);
    repo = lib.last (lib.splitString "/" manifest.repo);
    rev = "chrome-devtools-mcp-v${manifest.version}";
    inherit (manifest) hash;
  };

  npmBuildScript = "bundle";

  # TS2717: type conflict between devtools-frontend and @paulirish/trace_engine.
  # The error is in a .ts source (not .d.ts), so skipLibCheck doesn't help.
  # Use --noCheck (TS 5.5+) to skip type checking so the build can succeed.
  postPatch = ''
    cp -r --no-preserve=mode,ownership \
      ${devtools-frontend}/. third_party/devtools-frontend

    substituteInPlace package.json \
      --replace-fail '"build": "tsc &&' '"build": "tsc --noCheck &&'
  '';

  # Puppeteer tries to download Chrome during install; skip it since
  # users provide their own Chrome via --executablePath or --browserUrl.
  env.PUPPETEER_SKIP_DOWNLOAD = "1";

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Chrome DevTools MCP server for AI coding assistants";
    homepage = "https://github.com/ChromeDevTools/chrome-devtools-mcp";
    license = lib.licenses.asl20;
    mainProgram = "chrome-devtools-mcp";
  };
}
