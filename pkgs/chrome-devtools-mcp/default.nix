{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "chrome-devtools-mcp";
  version = "0.20.3";

  src = fetchFromGitHub {
    owner = "ChromeDevTools";
    repo = "chrome-devtools-mcp";
    rev = "${pname}-v${version}";
    hash = "sha256-Q2GogBX6FeS9SFxTdS6OMf1GqGpOSLzrAlsXX8xO1DI=";
  };

  npmDepsHash = "sha256-4u2pPw4wRXlf8zVZp7HsChjfXwm/9gntzKgo8GwTHsg=";
  npmBuildScript = "bundle";

  # TS2717: type conflict between chrome-devtools-frontend and @paulirish/trace_engine.
  # The error is in a .ts source (not .d.ts), so skipLibCheck doesn't help.
  # Use --noCheck (TS 5.5+) to skip type checking so the build can succeed.
  postPatch = ''
    substituteInPlace package.json \
      --replace-fail '"build": "tsc &&' '"build": "tsc --noCheck &&'
  '';

  # Puppeteer tries to download Chrome during install; skip it since
  # users provide their own Chrome via --executablePath or --browserUrl.
  env.PUPPETEER_SKIP_DOWNLOAD = "1";

  # nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'
  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Chrome DevTools MCP server for AI coding assistants";
    homepage = "https://github.com/ChromeDevTools/chrome-devtools-mcp";
    license = lib.licenses.asl20;
    mainProgram = "chrome-devtools-mcp";
  };
}
