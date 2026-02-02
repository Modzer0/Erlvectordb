#!/bin/bash

# ErlVectorDB Server Test Script
# Tests OAuth, MCP, and REST API functionality

set -e

echo "======================================"
echo "ErlVectorDB Server Test Suite"
echo "======================================"
echo ""

# Detect OAuth port (it varies between 8081 and 8083)
# Try both ports to find which one is active
if curl -s http://localhost:8081/health > /dev/null 2>&1; then
    OAUTH_PORT=8081
elif curl -s http://localhost:8083/health > /dev/null 2>&1; then
    OAUTH_PORT=8083
else
    # Default to 8081 and let it fail if neither works
    OAUTH_PORT=8081
fi
MCP_PORT=8080
REST_PORT=8082

# Test 1: OAuth Token Generation
echo "Test 1: OAuth Token Generation"
echo "--------------------------------------"
TOKEN_RESPONSE=$(curl -s -X POST http://localhost:$OAUTH_PORT/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024")

if echo "$TOKEN_RESPONSE" | jq -e '.access_token' > /dev/null 2>&1; then
    echo "✓ OAuth token generated successfully"
    ACCESS_TOKEN=$(echo "$TOKEN_RESPONSE" | jq -r '.access_token')
    echo "  Access Token: ${ACCESS_TOKEN:0:20}..."
    echo "  Token Type: $(echo "$TOKEN_RESPONSE" | jq -r '.token_type')"
    echo "  Expires In: $(echo "$TOKEN_RESPONSE" | jq -r '.expires_in') seconds"
else
    echo "✗ Failed to generate OAuth token"
    echo "  Response: $TOKEN_RESPONSE"
    exit 1
fi
echo ""

# Test 2: MCP Initialize
echo "Test 2: MCP Initialize Request"
echo "--------------------------------------"
MCP_INIT_REQUEST='{
  "jsonrpc": "2.0",
  "method": "initialize",
  "id": 1,
  "auth": {
    "type": "bearer",
    "token": "'$ACCESS_TOKEN'"
  }
}'

MCP_INIT_RESPONSE=$(echo "$MCP_INIT_REQUEST" | nc localhost $MCP_PORT 2>/dev/null || echo '{"error": "connection_failed"}')

if echo "$MCP_INIT_RESPONSE" | jq -e '.result.serverInfo.name' > /dev/null 2>&1; then
    echo "✓ MCP server initialized successfully"
    echo "  Server Name: $(echo "$MCP_INIT_RESPONSE" | jq -r '.result.serverInfo.name')"
    echo "  Version: $(echo "$MCP_INIT_RESPONSE" | jq -r '.result.serverInfo.version')"
    echo "  Protocol: $(echo "$MCP_INIT_RESPONSE" | jq -r '.result.protocolVersion')"
else
    echo "✗ MCP initialization failed"
    echo "  Response: $MCP_INIT_RESPONSE"
fi
echo ""

# Test 3: MCP Tools List
echo "Test 3: MCP Tools List Request"
echo "--------------------------------------"
MCP_TOOLS_REQUEST='{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "id": 2,
  "auth": {
    "type": "bearer",
    "token": "'$ACCESS_TOKEN'"
  }
}'

MCP_TOOLS_RESPONSE=$(echo "$MCP_TOOLS_REQUEST" | nc localhost $MCP_PORT 2>/dev/null || echo '{"error": "connection_failed"}')

if echo "$MCP_TOOLS_RESPONSE" | jq -e '.result.tools' > /dev/null 2>&1; then
    TOOL_COUNT=$(echo "$MCP_TOOLS_RESPONSE" | jq '.result.tools | length')
    echo "✓ MCP tools list retrieved successfully"
    echo "  Available Tools: $TOOL_COUNT"
    echo "$MCP_TOOLS_RESPONSE" | jq -r '.result.tools[].name' | while read tool; do
        echo "    - $tool"
    done
else
    echo "✗ Failed to retrieve MCP tools list"
    echo "  Response: $MCP_TOOLS_RESPONSE"
fi
echo ""

# Test 4: REST API Health Check
echo "Test 4: REST API Health Check"
echo "--------------------------------------"
HEALTH_RESPONSE=$(curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
  http://localhost:$REST_PORT/health 2>/dev/null || echo '{"error": "connection_failed"}')

if echo "$HEALTH_RESPONSE" | jq -e '.status' > /dev/null 2>&1; then
    echo "✓ REST API health check passed"
    echo "  Status: $(echo "$HEALTH_RESPONSE" | jq -r '.status')"
else
    echo "✗ REST API health check failed"
    echo "  Response: $HEALTH_RESPONSE"
fi
echo ""

# Test 5: OAuth Client Info
echo "Test 5: OAuth Client Info"
echo "--------------------------------------"
CLIENT_INFO=$(curl -s -H "Authorization: Bearer $ACCESS_TOKEN" \
  http://localhost:$OAUTH_PORT/oauth/client_info 2>/dev/null || echo '{"error": "connection_failed"}')

if echo "$CLIENT_INFO" | jq -e '.client_id' > /dev/null 2>&1; then
    echo "✓ Client info retrieved successfully"
    echo "  Client ID: $(echo "$CLIENT_INFO" | jq -r '.client_id')"
    echo "  Scopes: $(echo "$CLIENT_INFO" | jq -r '.scopes | join(", ")')"
else
    echo "✗ Failed to retrieve client info"
    echo "  Response: $CLIENT_INFO"
fi
echo ""

echo "======================================"
echo "Test Suite Complete"
echo "======================================"
echo ""
echo "Summary:"
echo "  OAuth Server: http://localhost:$OAUTH_PORT"
echo "  MCP Server: http://localhost:$MCP_PORT"
echo "  REST API: http://localhost:$REST_PORT"
echo ""
echo "Default Credentials:"
echo "  Client ID: admin"
echo "  Client Secret: admin_secret_2024"
echo ""
