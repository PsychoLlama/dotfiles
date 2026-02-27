{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "chrome-devtools-mcp";
  version = "0.18.1";

  src = fetchFromGitHub {
    owner = "ChromeDevTools";
    repo = "chrome-devtools-mcp";
    rev = "${pname}-v${version}";
    hash = "sha256-Tdgf3LjhSYKKZ46rfUJRQXuNjrjceezPUZfwarmlYp0=";
  };

  npmDepsHash = "sha256-zh7YYVhWwoj590nfKmoHHRt8v7+mBrsDvA7gPeKnMdE=";
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
    mainProgram = "chrome-devtools-mcp";
  };
}
