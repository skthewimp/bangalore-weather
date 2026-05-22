#!/usr/bin/env bash
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found. Install system R before running this script." >&2
  exit 1
fi

if [[ -f "$PROJECT_DIR/.Renviron" ]]; then
  export R_ENVIRON_USER="$PROJECT_DIR/.Renviron"
fi

cd "$PROJECT_DIR"
exec Rscript "$PROJECT_DIR/bangalore_weather_update.R"
