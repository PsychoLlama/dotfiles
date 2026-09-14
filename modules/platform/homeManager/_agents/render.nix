{
  lib,
  stdenvNoCC,
  linkFarm,
  formats,
  minijinja,
  agent,
  skill,
}:

let
  inherit (skill) name;

  json = formats.json { };
  yaml = formats.yaml { };

  vars = lib.mapAttrs (_: values: values.${agent}) skill.template.vars;
  context = json.generate "${name}-vars.json" vars;
  frontmatter = yaml.generate "${name}-metadata.yaml" skill.frontmatter;
  files = lib.mapAttrs (_: value: if lib.isAttrs value then value.${agent} else value) skill.files;

  rendered = stdenvNoCC.mkDerivation {
    name = "${name}-SKILL.md";
    dontUnpack = true;
    nativeBuildInputs = [ minijinja ];

    buildPhase = ''
      runHook preBuild
      minijinja-cli --strict --autoescape none ${template} ${context} > SKILL.md
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cp SKILL.md "$out"
      runHook postInstall
    '';
  };

  template = stdenvNoCC.mkDerivation {
    name = "${name}-template.md";
    dontUnpack = true;

    buildPhase = ''
      runHook preBuild

      {
        printf '%s\n' '---'
        cat ${frontmatter}
        printf '%s\n\n' '---'
        cat ${skill.template.body}
      } > template.md

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cp template.md "$out"
      runHook postInstall
    '';
  };
in

(linkFarm name files).overrideAttrs (old: {
  # Codex bug: symlinked directories are fine, but symlinked `SKILL.md` is
  # silently invisible. One could say "sighlently", because I sighed in
  # disappointment.
  buildCommand = old.buildCommand + ''
    cp ${rendered} "$out/SKILL.md"
  '';
})
