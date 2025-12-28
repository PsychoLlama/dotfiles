{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.litellm;
in

{
  options.psychollama.presets.services.litellm = {
    enable = lib.mkEnableOption "LiteLLM proxy for unified LLM API access";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.litellm-env.file = ./env.age;

    services.litellm = {
      enable = true;
      package = pkgs.unstable.litellm;
      host = lib.mkDefault "127.0.0.1";
      port = lib.mkDefault 4000;
      environmentFile = config.age.secrets.litellm-env.path;

      settings = {
        model_list = [
          # Anthropic
          {
            model_name = "claude-sonnet-4.5";
            litellm_params = {
              model = "anthropic/claude-sonnet-4-5-20250929";
              api_key = "os.environ/ANTHROPIC_API_KEY";
            };
          }
          {
            model_name = "claude-opus-4.5";
            litellm_params = {
              model = "anthropic/claude-opus-4-5-20251101";
              api_key = "os.environ/ANTHROPIC_API_KEY";
            };
          }

          # OpenAI
          {
            model_name = "gpt-5.2";
            litellm_params = {
              model = "openai/gpt-5.2";
              api_key = "os.environ/OPENAI_API_KEY";
            };
          }
          {
            model_name = "gpt-5.2-pro";
            litellm_params = {
              model = "openai/gpt-5.2-pro";
              api_key = "os.environ/OPENAI_API_KEY";
            };
          }

          # Gemini
          {
            model_name = "gemini-3-pro";
            litellm_params = {
              model = "gemini/gemini-3-pro-preview";
              api_key = "os.environ/GEMINI_API_KEY";
            };
          }
          {
            model_name = "gemini-3-flash";
            litellm_params = {
              model = "gemini/gemini-3-flash-preview";
              api_key = "os.environ/GEMINI_API_KEY";
            };
          }

          # Image Generation (Nano Banana)
          {
            model_name = "nano-banana";
            litellm_params = {
              model = "gemini/gemini-2.5-flash-image-preview";
              api_key = "os.environ/GEMINI_API_KEY";
            };
          }
          {
            model_name = "nano-banana-pro";
            litellm_params = {
              model = "gemini/gemini-3-pro-image-preview";
              api_key = "os.environ/GEMINI_API_KEY";
            };
          }
        ];

        litellm_settings = {
          drop_params = true;
          telemetry = false;
        };
      };
    };
  };
}
