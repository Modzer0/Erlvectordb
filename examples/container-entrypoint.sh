#!/bin/bash
set -e

# Container entrypoint script for ErlVectorDB with port management

# Function to log with timestamp
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"
}

# Function to handle graceful shutdown
graceful_shutdown() {
    log "Received shutdown signal, initiating graceful shutdown..."
    
    # Get graceful shutdown timeout from environment (default 30 seconds)
    TIMEOUT=${GRACEFUL_SHUTDOWN_TIMEOUT:-30}
    log "Graceful shutdown timeout: ${TIMEOUT} seconds"
    
    # Send SIGTERM to the Erlang VM
    if [ -n "$ERLANG_PID" ]; then
        log "Sending SIGTERM to Erlang VM (PID: $ERLANG_PID)"
        kill -TERM "$ERLANG_PID"
        
        # Wait for graceful shutdown
        for i in $(seq 1 $TIMEOUT); do
            if ! kill -0 "$ERLANG_PID" 2>/dev/null; then
                log "Erlang VM shut down gracefully"
                exit 0
            fi
            sleep 1
        done
        
        # Force kill if graceful shutdown failed
        log "Graceful shutdown timeout exceeded, forcing shutdown"
        kill -KILL "$ERLANG_PID" 2>/dev/null || true
    fi
    
    exit 0
}

# Set up signal handlers
trap graceful_shutdown SIGTERM SIGINT

# Container mode validation
log "Starting ErlVectorDB in container mode"
log "Container environment detected: CONTAINER=${CONTAINER}"
log "Bind all interfaces: BIND_ALL_INTERFACES=${BIND_ALL_INTERFACES}"
log "Port mapping logging: LOG_PORT_MAPPINGS=${LOG_PORT_MAPPINGS}"

# Port configuration logging
if [ "$LOG_PORT_MAPPINGS" = "true" ]; then
    log "Port configuration:"
    log "  MCP Server: ${MCP_SERVER_PORT:-8080} (external: ${MCP_SERVER_EXTERNAL_PORT:-unknown})"
    log "  OAuth Server: ${OAUTH_SERVER_PORT:-8081} (external: ${OAUTH_SERVER_EXTERNAL_PORT:-unknown})"
    log "  REST API Server: ${REST_API_SERVER_PORT:-8082} (external: ${REST_API_SERVER_EXTERNAL_PORT:-unknown})"
fi

# Development mode check
if [ "$ERLVECTORDB_DEV_MODE" = "true" ] || [ "$NODE_ENV" = "development" ]; then
    log "Development mode enabled"
    log "  Development timeout: ${ERLVECTORDB_DEV_TIMEOUT_MS:-2000}ms"
    CONFIG_FILE="config/dev.config"
else
    log "Production mode"
    CONFIG_FILE="config/container.config"
fi

# Check if custom config file is provided
if [ -n "$CONFIG_FILE_PATH" ]; then
    CONFIG_FILE="$CONFIG_FILE_PATH"
    log "Using custom config file: $CONFIG_FILE"
else
    log "Using default config file: $CONFIG_FILE"
fi

# Validate config file exists
if [ ! -f "$CONFIG_FILE" ]; then
    log "ERROR: Configuration file not found: $CONFIG_FILE"
    log "Available config files:"
    ls -la config/ || true
    exit 1
fi

# Pre-flight checks
log "Running pre-flight checks..."

# Check data directory
if [ ! -d "/data" ]; then
    log "Creating data directory: /data"
    mkdir -p /data
fi

if [ ! -w "/data" ]; then
    log "ERROR: Data directory is not writable: /data"
    exit 1
fi

# Check backup directory
if [ ! -d "/backups" ]; then
    log "Creating backup directory: /backups"
    mkdir -p /backups
fi

if [ ! -w "/backups" ]; then
    log "ERROR: Backup directory is not writable: /backups"
    exit 1
fi

# Check port availability (basic check)
check_port() {
    local port=$1
    local service=$2
    
    if command -v nc >/dev/null 2>&1; then
        if nc -z localhost "$port" 2>/dev/null; then
            log "WARNING: Port $port appears to be in use (for $service)"
            log "Port management system will handle conflicts automatically"
        else
            log "Port $port is available for $service"
        fi
    else
        log "netcat not available, skipping port availability check"
    fi
}

# Check configured ports
MCP_PORT=${MCP_SERVER_PORT:-8080}
OAUTH_PORT=${OAUTH_SERVER_PORT:-8081}
API_PORT=${REST_API_SERVER_PORT:-8082}

check_port "$MCP_PORT" "MCP Server"
check_port "$OAUTH_PORT" "OAuth Server"
check_port "$API_PORT" "REST API Server"

# Set up Erlang VM arguments
export ERL_FLAGS="-kernel logger_level info"

# Add container-specific VM arguments
if [ "$CONTAINER" = "true" ]; then
    export ERL_FLAGS="$ERL_FLAGS -kernel inet_dist_listen_min 9100 -kernel inet_dist_listen_max 9105"
fi

# Development mode VM arguments
if [ "$ERLVECTORDB_DEV_MODE" = "true" ]; then
    export ERL_FLAGS="$ERL_FLAGS +pc unicode -kernel logger_level debug"
fi

log "Erlang VM flags: $ERL_FLAGS"

# Start the application
log "Starting ErlVectorDB with config: $CONFIG_FILE"

case "$1" in
    "foreground")
        log "Starting in foreground mode"
        exec bin/erlvectordb foreground -config "$CONFIG_FILE" &
        ERLANG_PID=$!
        log "Erlang VM started with PID: $ERLANG_PID"
        
        # Wait for the process
        wait $ERLANG_PID
        ;;
    "console")
        log "Starting in console mode"
        exec bin/erlvectordb console -config "$CONFIG_FILE"
        ;;
    "daemon")
        log "Starting in daemon mode"
        exec bin/erlvectordb daemon -config "$CONFIG_FILE"
        ;;
    "start")
        log "Starting as background service"
        exec bin/erlvectordb start -config "$CONFIG_FILE"
        ;;
    *)
        log "Usage: $0 {foreground|console|daemon|start}"
        log "Default: foreground"
        exec bin/erlvectordb foreground -config "$CONFIG_FILE" &
        ERLANG_PID=$!
        log "Erlang VM started with PID: $ERLANG_PID"
        wait $ERLANG_PID
        ;;
esac