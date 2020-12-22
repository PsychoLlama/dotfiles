unset PATH
for buildInput in $buildInputs; do
  if [[ -d "$buildInput/bin" ]]; then
    export PATH="${PATH}${PATH:+:}${buildInput}/bin"
  fi
done

function phase_unpack {
  unzip "$src"
}

function phase_install {
  local target="$out/bin/fnm"

  mkdir -p "$(dirname "$target")"
  mv fnm "$target"
  chmod u+x "$target"
}

function build_fnm {
  phase_unpack
  phase_install
}
