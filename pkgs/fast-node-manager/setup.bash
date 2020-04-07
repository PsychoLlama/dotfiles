unset PATH
for buildInput in $buildInputs; do
  if [[ -d "$buildInput/bin" ]]; then
    export PATH="${PATH}${PATH:+:}${buildInput}/bin"
  fi
done

function phase_install {
  local target="$out/bin/fnm"

  mkdir -p "$(dirname "$target")"
  cp "$src/fnm" "$target"
  chmod u+x "$target"
}
