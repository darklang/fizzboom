#!/usr/bin/env bash

set -euo pipefail

dotnet build --configuration Release --runtime osx-x64
