{ config, lib, pkgs, ... }:

with lib;

# w3m is tremendously useful while debugging a NixOS issue that bricks your
# window manager, leaving you with only a terminal.

let cfg = config.presets.w3m;

in {
  options.presets.w3m.enable = mkEnableOption "Install and configure w3m";

  config = mkIf cfg.enable {
    programs.w3m = {
      enable = true;

      # w3m vim-like keymap file
      #   by @sansna
      #
      # Adapted from https://github.com/sansna/keymap.w3m
      keybindings = {
        ########## invalidate defaults ##########
        "K" = "NULL";
        "J" = "NULL";
        "SPC" = "NULL";
        "-" = "NULL";
        "+" = "NULL";
        "C-v" = "NULL";
        "ESC-v" = "NULL";
        "C-f" = "NULL";
        "C-b" = "NULL";
        "<" = "NULL";
        ">" = "NULL";
        "." = "NULL";
        "," = "NULL";
        "^" = "NULL";
        "C-a" = "NULL";
        "W" = "NULL";
        "^[[6~" = "NULL";
        "^[[5~" = "NULL";
        "g" = "NULL";
        "^[[1~" = "NULL";
        "^[[4~" = "NULL";
        "ESC-<" = "NULL";
        "ESC->" = "NULL";
        "[" = "NULL";
        "]" = "NULL";
        "^[[Z" = "NULL";
        "ESC-m" = "NULL";
        "(" = "NULL";
        ")" = "NULL";
        "C-m" = "NULL";
        "ESC-C-m" = "NULL";
        "ESC-w" = "NULL";
        "ESC-W" = "NULL";
        "C-s" = "NULL";
        "=" = "NULL";
        "ESC-l" = "NULL";
        "U" = "NULL";
        "V" = "NULL";
        "v" = "NULL";
        "R" = "NULL";
        "ESC-s" = "NULL";
        ":" = "NULL";
        "C-q" = "NULL";
        "}" = "NULL";
        "{" = "NULL";
        "ESC-a" = "NULL";
        "ESC-b" = "NULL";
        "c" = "NULL";
        "ESC-:" = "NULL";
        "C-h" = "NULL";
        "q" = "NULL";
        "Q" = "NULL";
        "C-w" = "NULL";
        "Z" = "NULL";
        "z" = "NULL";
        "ESC-g" = "NULL";
        "G" = "NULL";
        "ESC-TAB" = "NULL";
        "TAB" = "NULL";
        "I" = "NULL";
        "ESC-I" = "NULL";
        "i" = "NULL";
        "\";\"" = "NULL";
        "M" = "NULL";
        "ESC-M" = "NULL";
        "ESC-u" = "NULL";
        "@" = "NULL";
        "\"#\"" = "NULL";
        "|" = "NULL";
        "B" = "NULL";
        "s" = "NULL";
        "S" = "NULL";
        "E" = "NULL";
        "ESC-e" = "NULL";
        "C-l" = "NULL";
        "ESC-t" = "NULL";
        "C-@" = "NULL";
        "ESC-n" = "NULL";
        "ESC-p" = "NULL";
        "\\\"" = "NULL";
        "^[[2~" = "NULL";
        "^[[28~" = "NULL";
        "^[[E" = "NULL";
        "^[[L" = "NULL";
        "o" = "NULL";
        "C-k" = "NULL";
        "D" = "NULL";
        "m" = "NULL";
        "ESC-c" = "NULL";
        "ESC-o" = "NULL";
        "ESC-k" = "NULL";
        "\\\\" = "NULL";
        "!" = "NULL";
        "C-z" = "NULL";

        ########## command ##########
        "::" = "COMMAND";
        ":H" = "HELP";
        ":O" = "OPTIONS";
        ":d" = "DOWNLOAD_LIST";

        ########## history navigation ##########
        "L" = "NEXT";
        "H" = "PREV";
        ":p" = "SELECT_MENU";
        ":h" = "HISTORY";

        ########## scrolling ##########
        "C-e" = "UP";
        "C-y" = "DOWN";
        "gg" = "BEGIN";
        "C-d" = "NEXT_PAGE";
        "d" = "NEXT_PAGE";
        "C-u" = "PREV_PAGE";
        "u" = "PREV_PAGE";
        "zz" = "CENTER_V";

        ########## cursor ##########
        #"l" = "MOVE_RIGHT";
        "l" = "NEXT_TAB";
        #"h" = "MOVE_LEFT";
        "h" = "PREV_TAB";
        #"j" = "MOVE_DOWN1";
        "j" = "UP";
        #"k" = "MOVE_UP1";
        "k" = "DOWN";
        "RIGHT" = "MOVE_RIGHT";
        "LEFT" = "MOVE_LEFT";
        "DOWN" = "MOVE_DOWN1";
        "UP" = "MOVE_UP1";
        "0" = "LINE_BEGIN";
        "$" = "LINE_END";
        "w" = "NEXT_WORD";
        "b" = "PREV_WORD";

        ########## cursor history ##########
        "C-i" = "REDO";
        "C-o" = "UNDO";

        ########## navigation ##########
        "f" = "MOVE_LIST_MENU";
        "F" = "LIST_MENU";
        "C-n" = "NEXT_LINK";
        "C-p" = "PREV_LINK";
        ":l" = "PEEK_LINK";

        # reload
        "r" = "RELOAD";
        "C-r" = "RELOAD";

        # save/load 
        ":w" = "SAVE";
        ":W" = "PRINT";
        ":o" = "GOTO";
        ":e" = "LOAD";

        # jump
        "ESC-C-j" = "SUBMIT";
        "C-]" = "TAB_LINK";
        "C-j" = "GOTO_LINK";

        ########## info ##########
        "y" = "PEEK";
        "gC-g" = "INFO";
        "C-g" = "LINE_INFO";
        "gf" = "VIEW";

        ########## search ##########
        "/" = "ISEARCH";
        "?" = "ISEARCH_BACK";
        "n" = "SEARCH_NEXT";
        "N" = "SEARCH_PREV";

        ########## bookmarks ##########
        "a" = "ADD_BOOKMARK";
        ":b" = "VIEW_BOOKMARK";

        ########## tab ##########
        "x" = "CLOSE_TAB";
        "gh" = "GOTO http://www.google.com/en";
        "gH" = "TAB_GOTO http://www.google.com/en";
        "C-t" = "NEW_TAB";
        "gt" = "NEXT_TAB";
        "gT" = "PREV_TAB";
        "C-wL" = "TAB_RIGHT";
        "C-wH" = "TAB_LEFT";
        "t" = "TAB_GOTO";
        "T" = "TAB_MENU";

        ########## quit ##########
        "ZZ" = "EXIT";
        "C-Q" = "EXIT";
        "ZQ" = "QUIT";
        ":q" = "QUIT";
      };
    };
  };
}
