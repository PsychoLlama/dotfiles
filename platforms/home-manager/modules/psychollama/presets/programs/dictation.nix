{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.dictation;

  dictation = pkgs.writeShellApplication {
    name = "dictation";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.curl
      pkgs.pipewire
      pkgs.whisper-cpp
    ];
    text = ''
      MODEL_URL="https://huggingface.co/ggerganov/whisper.cpp/resolve/main"
      MODEL="''${DICTATION_MODEL:-${cfg.model}}"
      MODEL_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/whisper"
      MODEL_PATH="$MODEL_DIR/ggml-$MODEL.bin"

      # Download model if needed
      if [[ ! -f "$MODEL_PATH" ]]; then
        mkdir -p "$MODEL_DIR"
        echo -e "\033[36mDownloading whisper model ($MODEL)...\033[0m" >&2
        curl -L "$MODEL_URL/ggml-$MODEL.bin" -o "$MODEL_PATH"
      fi

      # Record audio to temp file
      audio_file="$(mktemp --suffix=.wav)"
      echo -e "\033[1;32m● Recording...\033[0m Press Enter to stop" >&2

      pw-record "$audio_file" &
      pid=$!
      read -r
      kill -TERM "$pid"
      sleep 0.2

      # Transcribe
      echo -e "\033[33m⏳ Transcribing...\033[0m" >&2
      result="$(whisper-cli -m "$MODEL_PATH" --no-prints --no-timestamps "$audio_file" 2>/dev/null)"
      result="''${result#"''${result%%[![:space:]]*}"}"  # trim leading whitespace

      rm -f "$audio_file"

      if [[ -z "$result" ]]; then
        echo -e "\033[31mNo speech detected\033[0m" >&2
      else
        echo -e "\033[32m✓\033[0m" >&2
        printf '%s' "$result"
      fi
    '';
  };
in

{
  options.psychollama.presets.programs.dictation = {
    enable = lib.mkEnableOption "Speech-to-text using local Whisper";

    model = lib.mkOption {
      type = lib.types.str;
      default = "base.en";
      description = "Whisper model to use (tiny.en, base.en, small.en, medium.en, large)";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ dictation ];
  };
}
