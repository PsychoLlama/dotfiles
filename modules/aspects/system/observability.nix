let
  # The collector's only listener. Loopback: nothing is exposed off-box.
  address = "127.0.0.1:4318";

  group = "observability";
  sink = "/var/log/observability";
in

# A local sink for observability data. Alloy and OTLP are implementation
# details; the shape of what lands in `sink` is expected to change.
{
  exports = {
    nixos =
      { host, ... }:

      let
        inherit (host.identity) username;
      in

      {
        services.alloy = {
          enable = true;

          extraFlags = [
            # `otelcol.exporter.file` is public-preview.
            "--stability.level=public-preview"

            # Alloy reports usage stats to Grafana by default.
            "--disable-reporting"
          ];
        };

        environment.etc."alloy/config.alloy".text = ''
          otelcol.receiver.otlp "claude_code" {
            http {
              endpoint = "${address}"

              // Raw API bodies push batches past the 20MiB default.
              max_request_body_size = "128MiB"
            }

            output {
              metrics = [otelcol.exporter.file.archive.input]
              logs    = [otelcol.exporter.file.archive.input]
              traces  = [otelcol.exporter.file.archive.input]
            }
          }

          otelcol.exporter.file "archive" {
            path   = "${sink}/claude-code.jsonl"
            format = "json"

            rotation {
              max_megabytes = 128
              max_backups   = 8
            }
          }
        '';

        # Alloy runs under `DynamicUser`, so the directory is shared by group
        # rather than owner. Setgid keeps rotated files readable.
        systemd.tmpfiles.settings.observability-sink.${sink}.d = {
          user = "root";
          inherit group;
          mode = "2770";
        };

        systemd.services.alloy.serviceConfig = {
          SupplementaryGroups = [ group ];

          # `DynamicUser` implies `ProtectSystem=strict`, so the sink is
          # read-only to the unit until it's punched out of the sandbox.
          ReadWritePaths = [ sink ];
        };

        users.groups.${group} = { };
        users.users.${username}.extraGroups = [ group ];
      };

    homeManager = {
      programs.claude-code.settings.env = {
        CLAUDE_CODE_ENABLE_TELEMETRY = "1";
        OTEL_METRICS_EXPORTER = "otlp";
        OTEL_LOGS_EXPORTER = "otlp";
        OTEL_EXPORTER_OTLP_PROTOCOL = "http/protobuf";
        OTEL_EXPORTER_OTLP_ENDPOINT = "http://${address}";

        # Defaults are 60s/5s. Shorter keeps the file current enough to poke at.
        OTEL_METRIC_EXPORT_INTERVAL = "10000";
        OTEL_LOGS_EXPORT_INTERVAL = "5000";

        OTEL_LOG_USER_PROMPTS = "1";
        OTEL_LOG_ASSISTANT_RESPONSES = "1";
        OTEL_LOG_TOOL_CONTENT = "1";
        OTEL_LOG_TOOL_DETAILS = "1";
        OTEL_LOG_RAW_API_BODIES = "1";

        # Default (61440) truncates mid-system-prompt, which makes the raw
        # bodies useless. Note this is the dominant cost of raw body capture.
        CLAUDE_CODE_OTEL_CONTENT_MAX_LENGTH = "1048576";
      };
    };
  };
}
