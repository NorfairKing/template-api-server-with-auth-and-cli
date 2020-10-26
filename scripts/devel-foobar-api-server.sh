#!/usr/bin/env bash

stack install foobar-api-server \
  --file-watch \
  --exec='./scripts/restart-foobar-api-server.sh'
