#!/usr/bin/env bash


killall foo-bar-api-server || true

foo-bar-api-server &
