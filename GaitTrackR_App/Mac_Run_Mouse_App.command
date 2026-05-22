#!/bin/bash
cd "$(dirname "$0")"

if ! command -v Rscript >/dev/null 2>&1; then
  osascript -e 'display dialog "Rscript not found. Please install R from https://cran.r-project.org and try again." buttons {"OK"}'
  exit 1
fi

Rscript run_app.R

