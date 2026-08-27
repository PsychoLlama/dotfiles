{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "chrome-devtools-mcp";
  version = "1.8.0";

  src = fetchFromGitHub {
    owner = "ChromeDevTools";
    repo = "chrome-devtools-mcp";
    rev = "${pname}-v${version}";
    # The build compiles devtools-frontend sources vendored as a submodule.
    fetchSubmodules = true;
    hash = "sha256-p9xClutSo2j1z+zSiXGLS3Gkak19d+YF5KmGWrMCOnE=";
  };

  npmDepsHash = "sha256-umqM+Av+clI+fB2Iq+7/Eos9avmnh8Q8rcl7JIv/bL0=";
  npmBuildScript = "bundle";

  # TS2717: type conflict between devtools-frontend and @paulirish/trace_engine.
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
