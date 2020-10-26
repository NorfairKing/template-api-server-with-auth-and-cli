#!/usr/bin/env bash


killall foobar-api-server || true

foobar-api-server &
