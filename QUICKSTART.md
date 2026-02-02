# ErlVectorDB Quick Start Guide

Get up and running with ErlVectorDB in 5 minutes!

## Prerequisites

- **Erlang/OTP 24+**: [Download](https://www.erlang.org/downloads)
- **rebar3**: [Install Guide](https://rebar3.org/docs/getting-started/)
- **Python 3.8+** (optional, for AI features): [Download](https://www.python.org/downloads/)

## Quick Start

### 1. Clone and Start

```bash
# Clone the repository
git clone https://github.com/yourusername/Erlvectordb.git
cd Erlvectordb

# Start the server (automatic setup)
./start-local.sh
```

The starter script will:
- ✅ Check prerequisites
- ✅ Compile the project
- ✅ Create necessary directories
- ✅ Check port availability
- ✅ Start all services

### 2. Verify Installation

Open a new terminal and run:

```bash
./test_server.sh
```

You should see all tests passing:
```
✓ OAuth token generated successfully
✓ MCP server initialized successfully
✓ MCP tools list retrieved (4 tools)
✓ REST API health check passed
✓ OAuth client info retrieved successfully
```

### 3. Try It Out

#### Get an OAuth Token

```bash
curl -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024"
```

#### Use the REST API

```bash
# Save your token
TOKEN="your_access_token_here"

# Create a vector store
curl -X POST http://localhost:8082/api/v1/stores \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"name": "my_first_store"}'

# Insert a vector
curl -X POST http://localhost:8082/api/v1/stores/my_first_store/vectors \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "id": "doc1",
    "vector": [1.0, 2.0, 3.0, 4.0, 5.0],
    "metadata": {"title": "My First Document"}
  }'

# Search for similar vectors
curl -X POST http://localhost:8082/api/v1/stores/my_first_store/search \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "vector": [1.1, 2.1, 3.1, 4.1, 5.1],
    "k": 5
  }'
```

## Using with Gemini CLI

### 1. Install Dependencies

```bash
pip install google-generativeai
```

### 2. Get Gemini API Key

Visit: https://makersuite.google.com/app/apikey

### 3. Configure Gemini CLI

Create `~/.config/gemini-cli/config.json`:

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "python",
      "args": ["/path/to/Erlvectordb/examples/gemini_mcp_client.py"],
      "env": {
        "GEMINI_API_KEY": "your-gemini-api-key",
        "ERLVECTORDB_HOST": "localhost",
        "ERLVECTORDB_PORT": "8080",
        "ERLVECTORDB_OAUTH_HOST": "localhost",
        "ERLVECTORDB_OAUTH_PORT": "8081",
        "ERLVECTORDB_CLIENT_ID": "admin",
        "ERLVECTORDB_CLIENT_SECRET": "admin_secret_2024"
      }
    }
  }
}
```

### 4. Use with Gemini

```bash
gemini-cli
```

Now you can use natural language:
- "Store this document about machine learning"
- "Find similar documents about AI"
- "What documents do I have?"

## Using with Claude Desktop

Add to your Claude Desktop config:

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "node",
      "args": ["/path/to/Erlvectordb/examples/mcp_client.js"],
      "env": {
        "ERLVECTORDB_HOST": "localhost",
        "ERLVECTORDB_PORT": "8080",
        "ERLVECTORDB_OAUTH_HOST": "localhost",
        "ERLVECTORDB_OAUTH_PORT": "8081",
        "ERLVECTORDB_CLIENT_ID": "admin",
        "ERLVECTORDB_CLIENT_SECRET": "admin_secret_2024"
      }
    }
  }
}
```

## Using the Erlang API

In the Erlang shell:

```erlang
% Create a store
erlvectordb:create_store(my_store).

% Insert vectors
erlvectordb:insert(my_store, <<"doc1">>, [1.0, 2.0, 3.0], #{title => "Document 1"}).
erlvectordb:insert(my_store, <<"doc2">>, [2.0, 3.0, 4.0], #{title => "Document 2"}).

% Search
{ok, Results} = erlvectordb:search(my_store, [1.1, 2.1, 3.1], 5).

% Get statistics
{ok, Stats} = erlvectordb:get_stats(my_store).
```

## Server Endpoints

| Service | Port | Endpoint |
|---------|------|----------|
| MCP Server | 8080 | `tcp://localhost:8080` |
| OAuth Server | 8081 | `http://localhost:8081` |
| REST API | 8082 | `http://localhost:8082` |

## Default Credentials

- **Client ID**: `admin`
- **Client Secret**: `admin_secret_2024`
- **Scopes**: `read`, `write`, `admin`

## Common Commands

```bash
# Start server
./start-local.sh

# Run tests
rebar3 ct

# Run test suite
./test_server.sh

# Compile only
rebar3 compile

# Clean build
rebar3 clean

# Start Erlang shell
rebar3 shell
```

## Troubleshooting

### Port Already in Use

```bash
# Check what's using the ports
lsof -i :8080
lsof -i :8081
lsof -i :8082

# Kill the process if needed
kill -9 <PID>
```

### Compilation Errors

```bash
# Clean and rebuild
rebar3 clean
rebar3 compile
```

### Connection Refused

Make sure the server is running:
```bash
# Check if processes are running
ps aux | grep beam

# Check logs
tail -f logs/startup_*.log
```

## Next Steps

- 📖 Read the [Full Documentation](README.md)
- 🔧 Check [MCP Setup Guide](docs/MCP_SETUP_GUIDE.md)
- 🧪 Explore [Examples](examples/)
- 🚀 Review [Test Results](TEST_RESULTS.md)

## Getting Help

- **Issues**: [GitHub Issues](https://github.com/yourusername/Erlvectordb/issues)
- **Documentation**: [README.md](README.md)
- **Examples**: [examples/](examples/)

## What's Next?

1. **Try the Examples**: Check out `examples/` directory
2. **Enable Clustering**: See distributed setup in README
3. **Configure Compression**: Optimize storage with compression
4. **Set Up Backups**: Configure automatic backups
5. **Integrate with Your App**: Use the REST API or MCP protocol

Happy vector searching! 🚀
