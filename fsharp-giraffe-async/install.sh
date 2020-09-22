#!/usr/bin/env bash

set -euo pipefail

dotnet tool restore
dotnet paket install
