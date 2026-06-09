{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "chrome-devtools-mcp";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "ChromeDevTools";
    repo = "chrome-devtools-mcp";
    rev = "${pname}-v${version}";
    hash = "sha256-eXK32N2oiWWnsPLZ5sdCc/tSnhkBsyFVNRVOv6WqclM=";
  };

  npmDepsHash = "sha256-TRw2HKllMnDWi5jx0/bpEpPC/iMnjvr1FUV50PA5DJA=";
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
