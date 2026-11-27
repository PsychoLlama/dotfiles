# One color palette for the machine, read by the programs that draw with it.
# `nix-colors` and `stylix` cover the same ground, but each landed on the
# wrong side of what I wanted: one does too little, the other reaches into
# programs I would rather configure myself.
{
  rhizome,
  units,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) host;

  colors = {
    black = t.str;
    red = t.str;
    green = t.str;
    yellow = t.str;
    blue = t.str;
    magenta = t.str;
    cyan = t.str;
    white = t.str;
  };
in

{
  # The two halves of a base16 palette.
  palette = define.fact (
    { one, ... }: {
      scope = host.scope;
      holds = one;
      allow-unread = true;

      type = {
        normal = colors;
        bright = colors;
      };
    }
  );

  values.palettes.one-dark = {
    normal = {
      black = "#1e1e1e";
      red = "#e06c75";
      green = "#98c379";
      yellow = "#e5c07b";
      blue = "#61afef";
      magenta = "#c678dd";
      cyan = "#56b6c2";
      white = "#abb2bf";
    };

    bright = {
      black = "#3e4451";
      red = "#ff7a85";
      green = "#a8d389";
      yellow = "#f0d08b";
      blue = "#71bfff";
      magenta = "#d688ed";
      cyan = "#66c6d2";
      white = "#ffffff";
    };
  };
}
