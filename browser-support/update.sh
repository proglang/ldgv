#!/usr/bin/env bash
#
# Updates the compiled JavaScript in `browser-support/static/all.js`.

set -e

if [ ! -f default.nix ] || [ ! -d browser-support ]; then
  echo "Please run this script from the project root."
  exit 1
fi

result="$(nix-build -A ghcjs.ldgv --no-out-link)"
cp "${result}/bin/ldgv.jsexe/all.js" browser-support/static/all.js
