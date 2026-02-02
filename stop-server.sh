#!/bin/bash

# ErlVectorDB Stop and Cleanup Script
# Stops all server processes and cleans up ports

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================"
echo "ErlVectorDB Stop & Cleanup"
echo -e "======================================${NC}"
echo ""

# Function to print colored messages
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Track if anything was stopped
STOPPED_SOMETHING=false

# Function to kill process by pattern
kill_process() {
    local pattern=$1
    local description=$2
    
    if pgrep -f "$pattern" > /dev/null 2>&1; then
        print_info "Stopping $description..."
        PIDS=$(pgrep -f "$pattern")
        
        for PID in $PIDS; do
            print_info "  Killing process $PID"
            kill $PID 2>/dev/null || kill -9 $PID 2>/dev/null || true
            STOPPED_SOMETHING=true
        done
        
        # Wait a moment for process to die
        sleep 1
        
        # Check if still running
        if pgrep -f "$pattern" > /dev/null 2>&1; then
            print_warning "  Process still running, forcing kill..."
            pkill -9 -f "$pattern" 2>/dev/null || true
            sleep 1
        fi
        
        # Verify it's stopped
        if ! pgrep -f "$pattern" > /dev/null 2>&1; then
            print_success "  $description stopped"
        else
            print_error "  Failed to stop $description"
        fi
    else
        print_info "$description not running"
    fi
}

# Function to check and free port
check_port() {
    local port=$1
    local service=$2
    
    print_info "Checking port $port ($service)..."
    
    # Check using lsof (macOS/Linux)
    if command -v lsof &> /dev/null; then
        if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
            PIDS=$(lsof -Pi :$port -sTCP:LISTEN -t)
            print_warning "  Port $port is in use by PID(s): $PIDS"
            
            for PID in $PIDS; do
                PROCESS_NAME=$(ps -p $PID -o comm= 2>/dev/null || echo "unknown")
                print_info "  Killing process $PID ($PROCESS_NAME)"
                kill $PID 2>/dev/null || kill -9 $PID 2>/dev/null || true
                STOPPED_SOMETHING=true
            done
            
            sleep 1
            
            # Verify port is free
            if ! lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
                print_success "  Port $port is now free"
            else
                print_error "  Port $port is still in use"
            fi
        else
            print_success "  Port $port is free"
        fi
    # Fallback to netstat
    elif command -v netstat &> /dev/null; then
        if netstat -an 2>/dev/null | grep -q ":$port.*LISTEN"; then
            print_warning "  Port $port appears to be in use"
            print_info "  Use 'lsof -i :$port' to identify the process"
        else
            print_success "  Port $port is free"
        fi
    else
        print_warning "  Cannot check port status (lsof/netstat not available)"
    fi
}

echo -e "${YELLOW}Stopping ErlVectorDB processes...${NC}"
echo ""

# Stop rebar3 shell processes
kill_process "rebar3 shell" "rebar3 shell"

# Stop beam.smp processes related to erlvectordb
kill_process "beam.smp.*erlvectordb" "Erlang VM (erlvectordb)"

# Stop any beam processes on our ports
kill_process "beam.smp.*8080" "Erlang VM on port 8080"
kill_process "beam.smp.*8081" "Erlang VM on port 8081"
kill_process "beam.smp.*8082" "Erlang VM on port 8082"

# Stop epmd (Erlang Port Mapper Daemon) if running
if pgrep -f "epmd" > /dev/null 2>&1; then
    print_info "Erlang Port Mapper Daemon (epmd) is running"
    read -p "Stop epmd? This may affect other Erlang applications (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        kill_process "epmd" "Erlang Port Mapper Daemon"
    fi
fi

echo ""
echo -e "${YELLOW}Checking and freeing ports...${NC}"
echo ""

# Check and free ports
check_port 8080 "MCP Server"
check_port 8081 "OAuth Server"
check_port 8082 "REST API"

echo ""
echo -e "${YELLOW}Cleaning up resources...${NC}"
echo ""

# Clean up any stale lock files
if [ -f ".erlang.cookie" ]; then
    print_info "Found .erlang.cookie file"
fi

# Clean up crash dumps
if ls erl_crash.dump* 1> /dev/null 2>&1; then
    print_info "Removing crash dumps..."
    rm -f erl_crash.dump*
    print_success "Crash dumps removed"
fi

# Clean up rebar3 crash dumps
if ls rebar3.crashdump* 1> /dev/null 2>&1; then
    print_info "Removing rebar3 crash dumps..."
    rm -f rebar3.crashdump*
    print_success "Rebar3 crash dumps removed"
fi

# Optional: Clean up data directories
echo ""
read -p "Clean up data directories? (data/, backups/, logs/) (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    print_warning "This will delete all stored data!"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        if [ -d "data" ]; then
            print_info "Removing data directory..."
            rm -rf data/
            print_success "Data directory removed"
        fi
        
        if [ -d "backups" ]; then
            print_info "Removing backups directory..."
            rm -rf backups/
            print_success "Backups directory removed"
        fi
        
        if [ -d "logs" ]; then
            print_info "Removing logs directory..."
            rm -rf logs/
            print_success "Logs directory removed"
        fi
    fi
fi

echo ""
echo -e "${BLUE}======================================"
echo "Cleanup Summary"
echo -e "======================================${NC}"
echo ""

if [ "$STOPPED_SOMETHING" = true ]; then
    print_success "Server processes stopped and cleaned up"
else
    print_info "No server processes were running"
fi

echo ""
print_info "Verifying cleanup..."
echo ""

# Final verification
ALL_CLEAR=true

if pgrep -f "rebar3 shell" > /dev/null 2>&1 || pgrep -f "beam.smp.*erlvectordb" > /dev/null 2>&1; then
    print_warning "Some processes may still be running"
    ALL_CLEAR=false
else
    print_success "No ErlVectorDB processes running"
fi

# Check ports one more time
PORTS_FREE=true
for port in 8080 8081 8082; do
    if command -v lsof &> /dev/null; then
        if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
            print_warning "Port $port is still in use"
            PORTS_FREE=false
            ALL_CLEAR=false
        fi
    fi
done

if [ "$PORTS_FREE" = true ]; then
    print_success "All ports are free"
fi

echo ""

if [ "$ALL_CLEAR" = true ]; then
    echo -e "${GREEN}✓ Cleanup complete! Server is fully stopped.${NC}"
else
    echo -e "${YELLOW}⚠ Cleanup complete with warnings. Check messages above.${NC}"
fi

echo ""
echo "To start the server again, run: ./start-local.sh"
echo "To check server status, run: ./check-status.sh"
echo ""
