#!/usr/bin/env bash

set -euo pipefail

dotnet publish --configuration Release --runtime osx-x64
