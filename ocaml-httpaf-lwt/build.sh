#!/usr/bin/env bash

set -euo pipefail

esy build --release $@

