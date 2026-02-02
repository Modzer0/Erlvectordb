#!/bin/bash

# ErlVectorDB Status Check Script
# Quickly check if the server is running and healthy

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================"
echo "ErlVectorDB Status Check"
echo -e "======================================${NC}"
echo ""

# Check if Erlang beam process is running
BEAM_RUNNING=false
if pgrep -f "beam.*erlvectordb" > /dev/null 2>&1; then
    BEAM_RUNNING=true
    PID=$(pgrep -f "beam.*erlvectordb")
elif pgrep -f "rebar3 shell" > /dev/null 2>&1; then
    BEAM_RUNNING=true
    PID=$(pgrep -f "rebar3 shell")
fi

if [ "$BEAM_RUNNING" = true ]; then
    echo -e "${GREEN}✓${NC} ErlVectorDB process is running"
    echo "  PID: $PID"
else
    echo -e "${RED}✗${NC} ErlVectorDB process is NOT running"
    echo ""
    echo "To start the server, run: ./start-local.sh"
    exit 1
fi

echo ""

# Check ports
check_port() {
    local port=$1
    local service=$2
    
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1 || netstat -an 2>/dev/null | grep -q ":$port.*LISTEN"; then
        echo -e "${GREEN}✓${NC} $service listening on port $port"
        return 0
    else
        echo -e "${RED}✗${NC} $service NOT listening on port $port"
        return 1
    fi
}

check_port 8080 "MCP Server    "
check_port 8081 "OAuth Server  "
check_port 8082 "REST API      "

echo ""

# Test OAuth endpoint
echo -e "${BLUE}Testing OAuth endpoint...${NC}"
if curl -s -f -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} OAuth server responding correctly"
else
    echo -e "${RED}✗${NC} OAuth server not responding"
fi

# Test MCP endpoint
echo -e "${BLUE}Testing MCP endpoint...${NC}"
if timeout 2 bash -c "echo '' | nc localhost 8080" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} MCP server accepting connections"
else
    echo -e "${YELLOW}⚠${NC} MCP server connection test inconclusive"
fi

# Test REST API endpoint
echo -e "${BLUE}Testing REST API endpoint...${NC}"
TOKEN=$(curl -s -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024" 2>/dev/null | \
  grep -o '"access_token":"[^"]*"' | cut -d'"' -f4)

if [ -n "$TOKEN" ]; then
    if curl -s -f -H "Authorization: Bearer $TOKEN" http://localhost:8082/health > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} REST API responding correctly"
    else
        echo -e "${RED}✗${NC} REST API not responding"
    fi
else
    echo -e "${RED}✗${NC} Could not get OAuth token for REST API test"
fi

echo ""
echo -e "${BLUE}======================================"
echo "Summary"
echo -e "======================================${NC}"
echo ""
echo "Server Endpoints:"
echo "  MCP Server:   http://localhost:8080"
echo "  OAuth Server: http://localhost:8081"
echo "  REST API:     http://localhost:8082"
echo ""
echo "Default Credentials:"
echo "  Client ID:     admin"
echo "  Client Secret: admin_secret_2024"
echo ""
echo "To run full test suite: ./test_server.sh"
echo "To stop the server: pkill -f 'beam.*erlvectordb'"
echo ""
