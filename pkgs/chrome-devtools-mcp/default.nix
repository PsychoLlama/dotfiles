{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "chrome-devtools-mcp";
  version = "0.15.1";

  src = fetchFromGitHub {
    owner = "ChromeDevTools";
    repo = "chrome-devtools-mcp";
    rev = "${pname}-v${version}";
    hash = "sha256-5y6oG/ubh7g0JJA6rt70Oi2hbjwTLB6kXL6z4m/ufOE=";
  };

  npmDepsHash = "sha256-fgREF9d5VNARtm0b86E3c/bfzOQPO+Fs1YkX+vfXYoE=";
  npmBuildScript = "bundle";

  # Puppeteer tries to download Chrome during install; skip it since
  # users provide their own Chrome via --executablePath or --browserUrl.
  env.PUPPETEER_SKIP_DOWNLOAD = "1";

  # nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'
  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Chrome DevTools MCP server for AI coding assistants";
    homepage = "https://github.com/ChromeDevTools/chrome-devtools-mcp";
    license = lib.licenses.asl20;
  };
}
