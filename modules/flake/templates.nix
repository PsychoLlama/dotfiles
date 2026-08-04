{
  flake.templates = {
    project = {
      description = "Flake environment with no assumptions";
      path = ../../templates/project;
    };

    typescript = {
      description = "Flake environment for building TypeScript projects";
      path = ../../templates/typescript;
    };

    rust = {
      description = "Flake environment for building Rust projects";
      path = ../../templates/rust;
    };

    nvim = {
      description = "Flake environment for building Neovim plugins";
      path = ../../templates/nvim;
    };
  };
}
