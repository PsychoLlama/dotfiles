set -e

unset PATH
for buildInput in $buildInputs; do
  export PATH="$buildInput/bin/${PATH:+:}${PATH[*]}"
done

cp --no-preserve all -R "$plugin/share/vim-plugins/coc-nvim" "$out"
