#!/usr/bin/env bash

stack install template-api-server \
  --file-watch \
  --exec='./scripts/restart.sh'
