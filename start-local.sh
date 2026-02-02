#!/bin/bash

# ErlVectorDB Local Starter Script
# This script sets up and starts ErlVectorDB for local development

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DATA_DIR="./data"
BACKUP_DIR="./backups"
LOG_DIR="./logs"

echo -e "${BLUE}======================================"
echo "ErlVectorDB Local Starter"
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

# Check if Erlang is installed
print_info "Checking prerequisites..."
if ! command -v erl &> /dev/null; then
    print_error "Erlang/OTP is not installed!"
    echo "Please install Erlang/OTP 24+ from: https://www.erlang.org/downloads"
    exit 1
fi

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    print_error "rebar3 is not installed!"
    echo "Please install rebar3 from: https://rebar3.org/docs/getting-started/"
    exit 1
fi

print_success "Prerequisites check passed"
echo ""

# Create necessary directories
print_info "Setting up directories..."
mkdir -p "$DATA_DIR"
mkdir -p "$BACKUP_DIR"
mkdir -p "$LOG_DIR"
print_success "Directories created"
echo ""

# Check if already compiled
if [ ! -d "_build" ]; then
    print_info "Compiling ErlVectorDB (first time setup)..."
    rebar3 compile
    print_success "Compilation complete"
else
    print_info "ErlVectorDB already compiled"
    read -p "Do you want to recompile? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        print_info "Recompiling..."
        rebar3 compile
        print_success "Recompilation complete"
    fi
fi
echo ""

# Check if ports are available
print_info "Checking port availability..."
check_port() {
    local port=$1
    local service=$2
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1 || netstat -an 2>/dev/null | grep -q ":$port.*LISTEN"; then
        print_warning "Port $port ($service) is already in use"
        return 1
    else
        print_success "Port $port ($service) is available"
        return 0
    fi
}

PORTS_OK=true
check_port 8080 "MCP Server" || PORTS_OK=false
check_port 8081 "OAuth Server" || PORTS_OK=false
check_port 8082 "REST API" || PORTS_OK=false

if [ "$PORTS_OK" = false ]; then
    echo ""
    print_warning "Some ports are in use. The server may fail to start."
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_info "Startup cancelled"
        exit 0
    fi
fi
echo ""

# Display configuration
print_info "Server Configuration:"
echo "  MCP Server:   http://localhost:8080"
echo "  OAuth Server: http://localhost:8081"
echo "  REST API:     http://localhost:8082"
echo ""
echo "  Default Credentials:"
echo "    Client ID:     admin"
echo "    Client Secret: admin_secret_2024"
echo "    Scopes:        read, write, admin"
echo ""
echo "  Data Directory:   $DATA_DIR"
echo "  Backup Directory: $BACKUP_DIR"
echo "  Log Directory:    $LOG_DIR"
echo ""

# Ask if user wants to run tests
read -p "Run tests before starting? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    print_info "Running tests..."
    if rebar3 ct; then
        print_success "All tests passed"
    else
        print_warning "Some tests failed, but continuing..."
    fi
    echo ""
fi

# Start the server
print_info "Starting ErlVectorDB..."
echo ""
print_success "Server is starting!"
echo ""
echo -e "${YELLOW}You will be left at the Erlang shell prompt${NC}"
echo -e "${YELLOW}Press Ctrl+C twice to stop the server${NC}"
echo -e "${YELLOW}Or type 'q().' in the Erlang shell to quit${NC}"
echo ""
echo "======================================"
echo ""

# Start the Erlang shell (interactive mode)
rebar3 shell --eval "application:ensure_all_started(erlvectordb)"
