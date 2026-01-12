#!/bin/bash

# ErlVectorDB + Gemini MCP Demo Setup Script
# This script helps set up the environment for the Gemini MCP demo

set -e

echo "🚀 ErlVectorDB + Gemini MCP Demo Setup"
echo "======================================"

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is required but not installed"
    exit 1
fi

echo "✅ Python 3 found: $(python3 --version)"

# Check if pip is available
if ! command -v pip3 &> /dev/null; then
    echo "❌ pip3 is required but not installed"
    exit 1
fi

echo "✅ pip3 found"

# Install required Python packages
echo "📦 Installing required Python packages..."
pip3 install google-generativeai requests

echo "✅ Python packages installed"

# Check for Gemini API key
if [ -z "$GEMINI_API_KEY" ]; then
    echo ""
    echo "🔑 Gemini API Key Setup"
    echo "======================"
    echo "You need a Google AI API key to run this demo."
    echo ""
    echo "1. Go to: https://makersuite.google.com/app/apikey"
    echo "2. Create a new API key"
    echo "3. Set the environment variable:"
    echo ""
    echo "   export GEMINI_API_KEY='your-api-key-here'"
    echo ""
    echo "4. Then run this script again"
    echo ""
    
    read -p "Do you have your API key ready? (y/n): " -n 1 -r
    echo
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        read -p "Enter your Gemini API key: " api_key
        export GEMINI_API_KEY="$api_key"
        echo "✅ API key set for this session"
        echo ""
        echo "💡 To make this permanent, add this to your ~/.bashrc or ~/.zshrc:"
        echo "   export GEMINI_API_KEY='$api_key'"
    else
        echo "❌ API key is required to run the demo"
        exit 1
    fi
else
    echo "✅ Gemini API key found"
fi

# Check if ErlVectorDB is running
echo ""
echo "🔍 Checking ErlVectorDB services..."

# Check MCP server
if nc -z localhost 8080 2>/dev/null; then
    echo "✅ MCP server is running on port 8080"
else
    echo "❌ MCP server is not running on port 8080"
    echo ""
    echo "Please start ErlVectorDB first:"
    echo "  cd /path/to/erlvectordb"
    echo "  rebar3 shell"
    echo ""
    exit 1
fi

# Check OAuth server
if nc -z localhost 8081 2>/dev/null; then
    echo "✅ OAuth server is running on port 8081"
else
    echo "❌ OAuth server is not running on port 8081"
    echo ""
    echo "Please ensure ErlVectorDB is fully started with OAuth enabled"
    exit 1
fi

# Test OAuth endpoint
echo "🔐 Testing OAuth connection..."
oauth_response=$(curl -s -X POST http://localhost:8081/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write admin" \
    2>/dev/null || echo "ERROR")

if [[ "$oauth_response" == *"access_token"* ]]; then
    echo "✅ OAuth authentication working"
else
    echo "❌ OAuth authentication failed"
    echo "Response: $oauth_response"
    exit 1
fi

echo ""
echo "🎉 Setup completed successfully!"
echo ""
echo "🚀 Ready to run the Gemini MCP demo:"
echo "   python3 examples/gemini_mcp_client.py"
echo ""
echo "📖 What the demo will do:"
echo "  • Connect to ErlVectorDB via MCP protocol"
echo "  • Use Gemini AI to generate text embeddings"
echo "  • Analyze documents with AI-powered categorization"
echo "  • Perform semantic search with AI explanations"
echo "  • Demonstrate intelligent vector operations"
echo ""

# Ask if user wants to run the demo now
read -p "Run the demo now? (y/n): " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo ""
    echo "🚀 Starting Gemini MCP Demo..."
    echo ""
    python3 examples/gemini_mcp_client.py
else
    echo ""
    echo "👋 Setup complete. Run the demo when ready with:"
    echo "   python3 examples/gemini_mcp_client.py"
fi