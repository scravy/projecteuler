#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Changing working directory to $DIR"
cd ${DIR}

make euler.jar
make euler

echo "Done."
