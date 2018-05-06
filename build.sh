#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Changing working directory to $DIR"
cd ${DIR}

ghc euler.hs -o euler

echo "building euler.jar..."
mvn -Dorg.slf4j.simpleLogger.defaultLogLevel=warn package
cp target/euler.jar .

echo "Done."
