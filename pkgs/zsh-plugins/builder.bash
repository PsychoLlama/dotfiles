export PATH="$core/bin"

target="$out/share/$pluginName"
mkdir -p "$out/share"
cp -R "$src" "$target"
