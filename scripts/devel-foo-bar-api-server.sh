#!/usr/bin/env bash

stack install foo-bar-api-server \
  --file-watch \
  --exec='./scripts/restart-foo-bar-api-server.sh'
