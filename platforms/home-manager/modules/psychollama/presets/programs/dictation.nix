{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.dictation;

  dictation =
    pkgs.writers.writeNuBin "dictation"
      {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          (lib.makeBinPath [
            pkgs.bash
            pkgs.coreutils
            pkgs.pipewire
            pkgs.util-linux
            pkgs.whisper-cpp
          ])
          "--set"
          "DICTATION_MODEL"
          cfg.model
        ];
      }
      # nu
      ''
        const MODEL_URL = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main"

        # Speech-to-text using local Whisper
        export def main [
          --model (-m): string  # Whisper model (tiny.en, base.en, small.en, medium.en, large)
        ] {
          let model = $model | default $env.DICTATION_MODEL
          let model_dir = ($env.XDG_DATA_HOME? | default $"($env.HOME)/.local/share") | path join "whisper"
          let model_path = $model_dir | path join $"ggml-($model).bin"

          # Download model if needed
          if not ($model_path | path exists) {
            mkdir $model_dir
            print --stderr $"(ansi cyan)Downloading whisper model \(($model)\)...(ansi reset)"
            http get $"($MODEL_URL)/ggml-($model).bin" | save $model_path
          }

          # Record audio to temp file
          let audio_file = mktemp --tmpdir --suffix .wav
          print --stderr $"(ansi green_bold)● Recording...(ansi reset) Press Enter to stop"

          # pw-record needs SIGTERM (not SIGKILL) to properly finalize the WAV header
          let pid_file = mktemp --tmpdir
          ^bash -c $"pw-record '($audio_file)' & echo $! > '($pid_file)'"
          input
          let pid = open $pid_file | str trim
          ^kill -TERM $pid
          sleep 200ms
          rm -f $pid_file

          # Transcribe
          print --stderr $"(ansi yellow)⏳ Transcribing...(ansi reset)"
          let result = whisper-cli -m $model_path --no-prints --no-timestamps $audio_file | str trim

          rm -f $audio_file

          if ($result | is-empty) {
            print --stderr $"(ansi red)No speech detected(ansi reset)"
          } else {
            print --stderr $"(ansi green)✓(ansi reset)"
            $result | str trim
          }
        }
      '';
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
