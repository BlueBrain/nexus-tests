#!/usr/bin/env bash
find /opt/binaries -mindepth 1 -maxdepth 1 -type d -not -name 4f22628b-6a83-47c6-bc13-6c9ab11b9dd7 -exec rm -r "{}" \;