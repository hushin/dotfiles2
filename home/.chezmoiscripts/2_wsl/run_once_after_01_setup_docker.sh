#!/bin/bash
set -euo pipefail

has() {
  type "$1" > /dev/null 2>&1
}

if ! has "docker"; then
  echo "install docker"
fi
