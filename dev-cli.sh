#!/bin/bash

# ErlVectorDB Development CLI
# This script provides easy access to development mode operations

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the project directory
cd "$SCRIPT_DIR"

# Ensure ebin directory exists and compile if needed
if [ ! -d "ebin" ] || [ ! -f "ebin/dev_cli.beam" ] || [ "src/dev_cli.erl" -nt "ebin/dev_cli.beam" ]; then
    echo "Compiling development CLI..."
    mkdir -p ebin
    erlc -o ebin src/*.erl
fi

# Set development mode if not already set
if [ -z "$ERLVECTORDB_DEV_MODE" ]; then
    export ERLVECTORDB_DEV_MODE=true
fi

# Run the Erlang CLI with all arguments passed through
if [ $# -eq 0 ]; then
    exec erl -pa ebin -noshell -eval "dev_cli:main([])" -s init stop
else
    # Convert shell arguments to Erlang list format
    ARGS=""
    for arg in "$@"; do
        if [ -z "$ARGS" ]; then
            ARGS="\"$arg\""
        else
            ARGS="$ARGS,\"$arg\""
        fi
    done
    exec erl -pa ebin -noshell -eval "dev_cli:main([$ARGS])" -s init stop
fi