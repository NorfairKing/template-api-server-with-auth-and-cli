#!/usr/bin/env bash


killall template-api-server || true

template-api-server &
