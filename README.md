# ErlVectorDB

A high-performance MCP (Model Context Protocol) vector database implemented in Erlang/OTP, designed to take full advantage of the Actor model and OTP supervision trees for fault-tolerant, concurrent vector operations.

## Features

- **OTP-Native Architecture**: Built on gen_servers and supervision trees for maximum fault tolerance
- **Distributed Clustering**: Horizontal scaling with automatic replication and failover
- **OAuth 2.1 Authentication**: Secure authentication with client credentials and refresh token flows
- **REST API**: Full REST API alongside MCP protocol for web integration
- **Vector Compression**: Multiple compression algorithms to reduce storage requirements
- **MCP Protocol Support**: Full Model Context Protocol implementation for AI/ML integration
- **Persistent Storage**: DETS-based persistence with ETS for fast in-memory operations
- **Backup & Recovery**: Full backup/restore capabilities with JSON export/import
- **Concurrent Vector Operations**: Leverages Erlang's lightweight processes for parallel operations
- **Multiple Distance Metrics**: Cosine similarity, Euclidean distance, Manhattan distance
- **Dynamic Store Management**: Create and manage multiple vector stores at runtime
- **Hot Code Reloading**: Update vector operations without downtime

## Architecture

```
erlvectordb_sup (Main Supervisor)
├── cluster_manager (Distributed Clustering)
├── oauth_server (OAuth 2.1 Authentication Server)
├── oauth_http_handler (OAuth HTTP Endpoints)
├── rest_api_server (REST API Server)
├── vector_store_sup (Dynamic Supervisor for Vector Stores)
│   ├── vector_store (Gen Server per store)
│   │   └── vector_persistence (DETS + ETS + Compression)
│   └── vector_store (Gen Server per store)
│       └── vector_persistence (DETS + ETS + Compression)
├── mcp_server (MCP Protocol Handler with OAuth)
└── vector_index_manager (Index Management)
```

## Quick Start

> 🚀 **New to ErlVectorDB?** Check out the [Quick Start Guide](QUICKSTART.md) for a 5-minute setup!

### Prerequisites

- Erlang/OTP 24+
- Rebar3

### Building

```bash
rebar3 compile
```

### Running

```bash
# Quick start with automatic setup
./start-local.sh

# Or manually
rebar3 shell
```

### Managing the Server

```bash
# Check server status
./check-status.sh

# Stop the server and cleanup
./stop-server.sh

# Run automated test suite
./test_server.sh
```

### Testing

```bash
# Run automated test suite
./test_server.sh

# Run Common Test suites
rebar3 ct
```

### Basic Usage

```erlang
% Start the application
erlvectordb:start().

% Register OAuth client (optional - default admin client is created)
erlvectordb:register_oauth_client(<<"my_client">>, <<"my_secret">>, #{
    scopes => [<<"read">>, <<"write">>]
}).

% Get OAuth access token
{ok, TokenResponse} = erlvectordb:get_oauth_token(<<"admin">>, <<"admin_secret_2024">>, [<<"read">>, <<"write">>]).
AccessToken = maps:get(access_token, TokenResponse).

% Create a regular vector store
erlvectordb:create_store(my_store).

% Create a distributed vector store (if clustering enabled)
erlvectordb:create_distributed_store(distributed_store, #{replication_factor => 2}).

% Insert vectors (with automatic compression if enabled)
Vector1 = [1.0, 2.0, 3.0],
erlvectordb:insert(my_store, <<"doc1">>, Vector1, #{title => "Document 1"}).

% Insert with explicit compression
erlvectordb:insert_compressed(my_store, <<"doc2">>, [2.0, 3.0, 4.0], #{title => "Document 2"}).

% Search for similar vectors
QueryVector = [1.1, 2.1, 3.1],
{ok, Results} = erlvectordb:search(my_store, QueryVector, 5).

% Get store statistics
{ok, Stats} = erlvectordb:get_stats(my_store).

% Benchmark compression algorithms
Algorithms = [quantization_8bit, quantization_4bit, zlib_compression],
BenchmarkResults = erlvectordb:benchmark_compression([1.0, 2.0, 3.0, 4.0], Algorithms).

% Clustering operations
erlvectordb:join_cluster('other_node@hostname').
{ok, ClusterStatus} = erlvectordb:get_cluster_status().
```

## Distributed Clustering

### Setup Clustering

Enable clustering in configuration:
```erlang
{cluster_enabled, true},
{node_name, 'erlvectordb@node1.example.com'},
{cluster_cookie, my_secure_cookie},
{replication_factor, 2}
```

### Cluster Operations

```erlang
% Join an existing cluster
erlvectordb:join_cluster('erlvectordb@node2.example.com').

% Create distributed stores
erlvectordb:create_distributed_store(my_distributed_store, #{
    replication_factor => 3
}).

% Check cluster status
{ok, Status} = erlvectordb:get_cluster_status().

% Leave cluster
erlvectordb:leave_cluster().
```

### Automatic Features

- **Replication**: Vectors automatically replicated across nodes
- **Failover**: Automatic failover when nodes go down
- **Load Balancing**: Queries distributed across available replicas
- **Consistency**: Eventually consistent with conflict resolution

## Vector Compression

### Supported Algorithms

- **8-bit Quantization**: Reduces precision to 8 bits per dimension
- **4-bit Quantization**: Reduces precision to 4 bits per dimension  
- **PCA Compression**: Principal Component Analysis dimensionality reduction
- **Zlib Compression**: General-purpose compression
- **Product Quantization**: Subvector quantization for large vectors

### Compression Usage

```erlang
% Enable compression globally
application:set_env(erlvectordb, compression_enabled, true).
application:set_env(erlvectordb, compression_algorithm, quantization_8bit).

% Compress specific vectors
{ok, Compressed} = erlvectordb:compress_vector([1.0, 2.0, 3.0], quantization_8bit).

% Benchmark compression algorithms
Vector = lists:seq(1.0, 100.0, 1.0),
Algorithms = [quantization_8bit, quantization_4bit, zlib_compression],
Results = erlvectordb:benchmark_compression(Vector, Algorithms).

% Results include compression_ratio, compression_time, decompression_time, accuracy_loss
```

### Compression Benefits

- **Storage Reduction**: 50-90% storage savings depending on algorithm
- **Network Efficiency**: Faster replication and backup operations
- **Memory Usage**: Reduced RAM requirements for large datasets
- **Configurable Trade-offs**: Balance between compression ratio and accuracy

## REST API

The system provides a full REST API alongside the MCP protocol:

### Endpoints

**Store Management:**
- `POST /api/v1/stores` - Create store
- `GET /api/v1/stores` - List stores  
- `DELETE /api/v1/stores/{name}` - Delete store

**Vector Operations:**
- `POST /api/v1/stores/{name}/vectors` - Insert vector
- `POST /api/v1/stores/{name}/search` - Search vectors
- `GET /api/v1/stores/{name}/stats` - Get store statistics

**Cluster Management:**
- `GET /api/v1/cluster/status` - Cluster status
- `POST /api/v1/cluster/join` - Join cluster

### REST API Examples

```bash
# Get OAuth token
curl -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write"

# Create store
curl -X POST http://localhost:8082/api/v1/stores \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"name": "my_store"}'

# Insert vector
curl -X POST http://localhost:8082/api/v1/stores/my_store/vectors \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"id": "doc1", "vector": [1.0, 2.0, 3.0], "metadata": {"title": "Document 1"}}'

# Search vectors
curl -X POST http://localhost:8082/api/v1/stores/my_store/search \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"vector": [1.1, 2.1, 3.1], "k": 5}'
```

## OAuth 2.1 Authentication

### Default Credentials

The system creates a default admin client on startup:
- **Client ID**: `admin`
- **Client Secret**: `admin_secret_2024`
- **Scopes**: `read`, `write`, `admin`

### Getting Access Tokens

#### Using Erlang API:
```erlang
{ok, TokenResponse} = erlvectordb:get_oauth_token(
    <<"admin">>, 
    <<"admin_secret_2024">>, 
    [<<"read">>, <<"write">>]
).
AccessToken = maps:get(access_token, TokenResponse).
```

#### Using HTTP API:
```bash
curl -X POST http://localhost:8081/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write"
```

### Registering New Clients

```erlang
erlvectordb:register_oauth_client(<<"my_app">>, <<"secure_secret">>, #{
    scopes => [<<"read">>, <<"write">>],
    grant_types => [<<"client_credentials">>, <<"refresh_token">>]
}).
```

### Scope-Based Permissions

- **`read`**: Search vectors, get statistics
- **`write`**: Insert/delete vectors, sync stores
- **`admin`**: Backup/restore operations, client management

## AI Integration

### Gemini AI Integration

ErlVectorDB includes AI-powered features using Google's Gemini AI for semantic search and intelligent document analysis:

```bash
# Setup Gemini integration
export GEMINI_API_KEY='your-gemini-api-key'
pip install google-generativeai
bash examples/setup_gemini_demo.sh

# Run AI-enhanced demo
python examples/gemini_mcp_client.py
```

**AI Features:**
- **Semantic Embeddings**: Generate vector embeddings using Gemini AI
- **Smart Document Analysis**: Automatic categorization and metadata extraction
- **Intelligent Search**: Natural language queries with AI explanations
- **Content Understanding**: Sentiment analysis and topic extraction

**Example Usage:**
```python
from examples.gemini_mcp_client import GeminiMCPClient

client = GeminiMCPClient(gemini_api_key="your-key")
client.connect_to_vectordb()

# AI-powered document insertion with automatic analysis
result = client.smart_insert('ai_store', 'healthcare_doc', 
    'AI is revolutionizing medical diagnosis and treatment...')

# Semantic search with natural language
search = client.smart_search('medical AI applications', 'ai_store')
print(search['explanation'])  # AI explains why results are relevant
```

### Gemini CLI Configuration

To use ErlVectorDB with the Gemini CLI, you need to configure it as an MCP server.

#### Step 1: Get Your Gemini API Key

Get your API key from: https://makersuite.google.com/app/apikey

#### Step 2: Configure Gemini CLI

Create or edit your Gemini CLI configuration file:

**Location**: `~/.config/gemini-cli/config.json` (Linux/macOS) or `%USERPROFILE%\.config\gemini-cli\config.json` (Windows)

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "python",
      "args": ["/path/to/Erlvectordb/examples/gemini_mcp_server.py"],
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

**Important**: Replace `/path/to/Erlvectordb` with the actual path to your ErlVectorDB installation.

For detailed configuration options, see [Gemini MCP Server Configuration Guide](examples/GEMINI_MCP_SERVER_CONFIG.md).

#### Step 3: Start ErlVectorDB

```bash
# Using the local starter script (recommended)
./start-local.sh

# Or manually
rebar3 shell --eval "application:ensure_all_started(erlvectordb)"
```

#### Step 4: Use with Gemini CLI

```bash
# Start Gemini CLI
gemini-cli

# The ErlVectorDB MCP server will be automatically available
# You can now use natural language to interact with your vector database:

> "Store this document about machine learning in the database"
> "Find similar documents about AI"
> "What documents do I have about healthcare?"
```

#### Troubleshooting Gemini CLI Connection

If you get "Connection closed" errors:

1. **Verify ErlVectorDB is running**:
   ```bash
   ./check-status.sh
   ```

2. **Test the MCP server directly**:
   ```bash
   echo '{"jsonrpc":"2.0","method":"initialize","id":1,"params":{}}' | \
     python examples/gemini_mcp_server.py
   ```

3. **Check the logs**:
   The MCP server logs to stderr, so you can see errors in the Gemini CLI output.

4. **Verify the path in config**:
   Make sure the path to `gemini_mcp_server.py` is absolute and correct.

5. **Test OAuth connectivity**:
   ```bash
   curl -X POST http://localhost:8081/oauth/token \
     -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write"
   ```

6. **Check Python dependencies**:
   ```bash
   python3 -c "import requests; print('requests OK')"
   ```

7. **Enable debug logging**:
   ```bash
   export ERLVECTORDB_LOG_LEVEL=DEBUG
   python examples/gemini_mcp_server.py
   ```

#### Common Gemini MCP Server Issues

**Issue: "Connection timeout"**
- Symptom: Server fails to connect to ErlVectorDB
- Solution: Increase timeout in environment variables:
  ```bash
  export ERLVECTORDB_SOCKET_TIMEOUT=60
  ```

**Issue: "Authentication error"**
- Symptom: OAuth token acquisition fails
- Solution: Verify OAuth server is running and credentials are correct:
  ```bash
  # Check OAuth server
  curl http://localhost:8081/oauth/token \
    -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write"
  ```

**Issue: "Parse error"**
- Symptom: JSON parsing fails
- Solution: Ensure requests are properly formatted single-line JSON with newlines

**Issue: "Connection closed by remote host"**
- Symptom: ErlVectorDB closes connection unexpectedly
- Solution: Check ErlVectorDB logs for errors, verify network connectivity

**Issue: "Incomplete message"**
- Symptom: Large responses are truncated
- Solution: Increase buffer size:
  ```bash
  export ERLVECTORDB_BUFFER_SIZE=16384
  ```

#### Gemini MCP Server Configuration Options

The `gemini_mcp_server.py` script supports the following environment variables:

**Connection Settings:**
- `ERLVECTORDB_HOST` - MCP server host (default: localhost)
- `ERLVECTORDB_PORT` - MCP server port (default: 8080)
- `ERLVECTORDB_OAUTH_HOST` - OAuth server host (default: localhost)
- `ERLVECTORDB_OAUTH_PORT` - OAuth server port (default: 8081)

**Authentication:**
- `ERLVECTORDB_CLIENT_ID` - OAuth client ID (default: admin)
- `ERLVECTORDB_CLIENT_SECRET` - OAuth client secret (default: admin_secret_2024)

**Socket Configuration:**
- `ERLVECTORDB_SOCKET_TIMEOUT` - Socket timeout in seconds (default: 30)
- `ERLVECTORDB_BUFFER_SIZE` - Socket buffer size in bytes (default: 8192)

**Reconnection Settings:**
- `ERLVECTORDB_MAX_RECONNECT_ATTEMPTS` - Max reconnection attempts (default: 3)
- `ERLVECTORDB_RECONNECT_DELAY` - Initial reconnection delay in seconds (default: 1.0)

**OAuth Retry Settings:**
- `ERLVECTORDB_OAUTH_MAX_RETRIES` - Max OAuth token request retries (default: 3)
- `ERLVECTORDB_OAUTH_INITIAL_BACKOFF` - Initial backoff delay in seconds (default: 1.0)
- `ERLVECTORDB_OAUTH_MAX_BACKOFF` - Maximum backoff delay in seconds (default: 30.0)
- `ERLVECTORDB_OAUTH_BACKOFF_MULTIPLIER` - Backoff multiplier (default: 2.0)

**Logging:**
- `ERLVECTORDB_LOG_LEVEL` - Log level: DEBUG, INFO, WARNING, ERROR, CRITICAL (default: INFO)

**Example Configuration:**
```bash
# High-latency network configuration
export ERLVECTORDB_SOCKET_TIMEOUT=60
export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=5
export ERLVECTORDB_RECONNECT_DELAY=2.0

# Large message handling
export ERLVECTORDB_BUFFER_SIZE=32768

# Debug mode
export ERLVECTORDB_LOG_LEVEL=DEBUG

# Run server
python examples/gemini_mcp_server.py
```

#### Gemini MCP Server Usage Examples

**Basic Usage with Gemini CLI:**

Once configured, you can use natural language with Gemini CLI:

```bash
# Start Gemini CLI
gemini-cli

# Example interactions:
> "Create a vector store called 'documents'"
> "Insert a vector [1.0, 2.0, 3.0] with id 'doc1' into the documents store"
> "Search for vectors similar to [1.1, 2.1, 3.1] in the documents store"
> "List all available tools"
```

**Direct Testing (without Gemini CLI):**

Test the MCP server directly via stdin/stdout:

```bash
# Test initialize
echo '{"jsonrpc":"2.0","method":"initialize","id":1,"params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}}}}' | \
  python examples/gemini_mcp_server.py

# Test tools/list
echo '{"jsonrpc":"2.0","method":"tools/list","id":2,"params":{}}' | \
  python examples/gemini_mcp_server.py

# Test tools/call
echo '{"jsonrpc":"2.0","method":"tools/call","id":3,"params":{"name":"create_store","arguments":{"name":"test_store"}}}' | \
  python examples/gemini_mcp_server.py
```

**Programmatic Usage:**

Use the server components in your own Python code:

```python
from examples.gemini_mcp_server import MCPServer, ServerConfig

# Create configuration
config = ServerConfig.from_environment()

# Or customize configuration
config = ServerConfig(
    erlvectordb_host='localhost',
    erlvectordb_port=8080,
    oauth_host='localhost',
    oauth_port=8081,
    client_id='admin',
    client_secret='admin_secret_2024',
    socket_timeout=60,
    log_level='DEBUG'
)

# Validate configuration
config.validate()

# Create and run server
server = MCPServer(config)
exit_code = server.run()
```

**Component-Level Usage:**

Use individual components for custom integrations:

```python
from examples.gemini_mcp_server import (
    SocketHandler, OAuthManager, RequestRouter, 
    StdioHandler, ServerConfig
)

# Create configuration
config = ServerConfig.from_environment()

# OAuth token management
oauth_manager = OAuthManager(config)
token = oauth_manager.get_token()
print(f"Access token: {token}")

# Socket communication
socket_handler = SocketHandler(
    host=config.erlvectordb_host,
    port=config.erlvectordb_port
)
socket_handler.connect()

# Send request
request = {
    'jsonrpc': '2.0',
    'method': 'tools/list',
    'params': {},
    'id': 1,
    'auth': oauth_manager.get_auth_dict()
}
socket_handler.send_message(request)
response = socket_handler.receive_message()
print(f"Response: {response}")

# Cleanup
socket_handler.close()
```

#### Alternative: Direct Python Script

If you prefer to use the Python client directly for demos:

```bash
# Set environment variables
export GEMINI_API_KEY='your-gemini-api-key'
export ERLVECTORDB_HOST='localhost'
export ERLVECTORDB_PORT='8080'
export ERLVECTORDB_OAUTH_HOST='localhost'
export ERLVECTORDB_OAUTH_PORT='8081'
export ERLVECTORDB_CLIENT_ID='admin'
export ERLVECTORDB_CLIENT_SECRET='admin_secret_2024'

# Run the demo client (not for Gemini CLI)
python examples/gemini_mcp_client.py
```

**Claude Desktop Integration:**
```json
{
  "mcpServers": {
    "erlvectordb-ai": {
      "command": "python",
      "args": ["examples/gemini_mcp_client.py"],
      "env": {
        "GEMINI_API_KEY": "${GEMINI_API_KEY}",
        "ERLVECTORDB_HOST": "localhost",
        "ERLVECTORDB_PORT": "8080"
      }
    }
  }
}
```

## MCP Integration

The database exposes an MCP server on port 8080 (configurable) with OAuth 2.1 authentication. OAuth server runs on port 8081.

> 📖 **Detailed Setup Guide**: See [MCP Setup Guide](docs/MCP_SETUP_GUIDE.md) for complete configuration and connection instructions.

### Quick MCP Setup

1. **Start ErlVectorDB**:
   ```bash
   rebar3 shell
   ```

2. **Test Connection**:
   ```bash
   # Get OAuth token
   curl -X POST http://localhost:8081/oauth/token \
     -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write"
   
   # Test MCP connection
   telnet localhost 8080
   ```

3. **Run Example Client**:
   ```bash
   node examples/mcp_client.js
   # or
   python examples/mcp_client.py
   # or AI-enhanced with Gemini
   export GEMINI_API_KEY='your-key'
   python examples/gemini_mcp_client.py
   ```

### MCP Server Configuration

#### Basic Configuration
```erlang
% In config/sys.config
[
    {erlvectordb, [
        {mcp_port, 8080},              % MCP server port
        {oauth_enabled, true},         % Enable OAuth authentication
        {oauth_port, 8081},           % OAuth server port
        % ... other settings
    ]}
].
```

#### Disable Authentication (Development Only)
```erlang
{oauth_enabled, false}  % Disables OAuth - NOT recommended for production
```

### MCP Client Configuration

#### For Claude Desktop

Add to your Claude Desktop configuration file:

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
**Windows**: `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "node",
      "args": ["/path/to/mcp-client.js"],
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

#### For Other MCP Clients

Configure your MCP client to connect to:
- **MCP Endpoint**: `tcp://localhost:8080`
- **OAuth Endpoint**: `http://localhost:8081/oauth/token`

### MCP Client Implementation

Here's a complete Node.js MCP client example:

```javascript
// mcp-client.js
const net = require('net');
const https = require('https');

class ErlVectorDBClient {
    constructor(options = {}) {
        this.host = options.host || 'localhost';
        this.port = options.port || 8080;
        this.oauthHost = options.oauthHost || 'localhost';
        this.oauthPort = options.oauthPort || 8081;
        this.clientId = options.clientId || 'admin';
        this.clientSecret = options.clientSecret || 'admin_secret_2024';
        this.accessToken = null;
        this.socket = null;
    }

    async getAccessToken() {
        const postData = new URLSearchParams({
            grant_type: 'client_credentials',
            client_id: this.clientId,
            client_secret: this.clientSecret,
            scope: 'read write admin'
        }).toString();

        const options = {
            hostname: this.oauthHost,
            port: this.oauthPort,
            path: '/oauth/token',
            method: 'POST',
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded',
                'Content-Length': Buffer.byteLength(postData)
            }
        };

        return new Promise((resolve, reject) => {
            const req = http.request(options, (res) => {
                let data = '';
                res.on('data', (chunk) => data += chunk);
                res.on('end', () => {
                    try {
                        const response = JSON.parse(data);
                        if (response.access_token) {
                            this.accessToken = response.access_token;
                            resolve(response.access_token);
                        } else {
                            reject(new Error('No access token received'));
                        }
                    } catch (error) {
                        reject(error);
                    }
                });
            });

            req.on('error', reject);
            req.write(postData);
            req.end();
        });
    }

    async connect() {
        if (!this.accessToken) {
            await this.getAccessToken();
        }

        return new Promise((resolve, reject) => {
            this.socket = net.createConnection(this.port, this.host, () => {
                console.log('Connected to ErlVectorDB MCP server');
                resolve();
            });

            this.socket.on('error', reject);
            this.socket.on('data', (data) => {
                try {
                    const response = JSON.parse(data.toString());
                    this.handleResponse(response);
                } catch (error) {
                    console.error('Failed to parse response:', error);
                }
            });
        });
    }

    async sendRequest(method, params = {}, id = 1) {
        const request = {
            jsonrpc: '2.0',
            method: method,
            params: params,
            id: id,
            auth: {
                type: 'bearer',
                token: this.accessToken
            }
        };

        return new Promise((resolve, reject) => {
            this.responseHandlers = this.responseHandlers || {};
            this.responseHandlers[id] = { resolve, reject };

            const requestData = JSON.stringify(request);
            this.socket.write(requestData);
        });
    }

    handleResponse(response) {
        if (response.id && this.responseHandlers[response.id]) {
            const handler = this.responseHandlers[response.id];
            delete this.responseHandlers[response.id];

            if (response.error) {
                handler.reject(new Error(response.error.message));
            } else {
                handler.resolve(response.result);
            }
        }
    }

    // MCP Protocol Methods
    async initialize() {
        return this.sendRequest('initialize', {
            protocolVersion: '2024-11-05',
            capabilities: {
                tools: {}
            },
            clientInfo: {
                name: 'erlvectordb-client',
                version: '1.0.0'
            }
        });
    }

    async listTools() {
        return this.sendRequest('tools/list');
    }

    async callTool(name, arguments) {
        return this.sendRequest('tools/call', {
            name: name,
            arguments: arguments
        });
    }

    // Vector Database Operations
    async createStore(name) {
        return this.callTool('create_store', { name: name });
    }

    async insertVector(store, id, vector, metadata = {}) {
        return this.callTool('insert_vector', {
            store: store,
            id: id,
            vector: vector,
            metadata: metadata
        });
    }

    async searchVectors(store, vector, k = 10) {
        return this.callTool('search_vectors', {
            store: store,
            vector: vector,
            k: k
        });
    }

    async syncStore(store) {
        return this.callTool('sync_store', { store: store });
    }

    async backupStore(store, backupName) {
        return this.callTool('backup_store', {
            store: store,
            backup_name: backupName
        });
    }

    async listBackups() {
        return this.callTool('list_backups', {});
    }

    disconnect() {
        if (this.socket) {
            this.socket.end();
            this.socket = null;
        }
    }
}

// Usage Example
async function main() {
    const client = new ErlVectorDBClient({
        host: process.env.ERLVECTORDB_HOST || 'localhost',
        port: parseInt(process.env.ERLVECTORDB_PORT) || 8080,
        oauthHost: process.env.ERLVECTORDB_OAUTH_HOST || 'localhost',
        oauthPort: parseInt(process.env.ERLVECTORDB_OAUTH_PORT) || 8081,
        clientId: process.env.ERLVECTORDB_CLIENT_ID || 'admin',
        clientSecret: process.env.ERLVECTORDB_CLIENT_SECRET || 'admin_secret_2024'
    });

    try {
        await client.connect();
        await client.initialize();
        
        const tools = await client.listTools();
        console.log('Available tools:', tools);

        // Create a store
        await client.createStore('test_store');
        
        // Insert a vector
        await client.insertVector('test_store', 'doc1', [1.0, 2.0, 3.0], {
            title: 'Test Document'
        });
        
        // Search for similar vectors
        const results = await client.searchVectors('test_store', [1.1, 2.1, 3.1], 5);
        console.log('Search results:', results);
        
    } catch (error) {
        console.error('Error:', error);
    } finally {
        client.disconnect();
    }
}

if (require.main === module) {
    main();
}

module.exports = ErlVectorDBClient;
```

### Python MCP Client

```python
# mcp_client.py
import json
import socket
import requests
from typing import Dict, List, Any, Optional

class ErlVectorDBClient:
    def __init__(self, host='localhost', port=8080, oauth_host='localhost', 
                 oauth_port=8081, client_id='admin', client_secret='admin_secret_2024'):
        self.host = host
        self.port = port
        self.oauth_host = oauth_host
        self.oauth_port = oauth_port
        self.client_id = client_id
        self.client_secret = client_secret
        self.access_token = None
        self.socket = None
        self.request_id = 1

    def get_access_token(self) -> str:
        """Get OAuth access token"""
        url = f'http://{self.oauth_host}:{self.oauth_port}/oauth/token'
        data = {
            'grant_type': 'client_credentials',
            'client_id': self.client_id,
            'client_secret': self.client_secret,
            'scope': 'read write admin'
        }
        
        response = requests.post(url, data=data)
        response.raise_for_status()
        
        token_data = response.json()
        self.access_token = token_data['access_token']
        return self.access_token

    def connect(self):
        """Connect to MCP server"""
        if not self.access_token:
            self.get_access_token()
        
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((self.host, self.port))

    def send_request(self, method: str, params: Dict = None, request_id: int = None) -> Dict:
        """Send MCP request"""
        if request_id is None:
            request_id = self.request_id
            self.request_id += 1

        request = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params or {},
            'id': request_id,
            'auth': {
                'type': 'bearer',
                'token': self.access_token
            }
        }

        request_data = json.dumps(request).encode('utf-8')
        self.socket.send(request_data)

        # Receive response
        response_data = self.socket.recv(4096)
        response = json.loads(response_data.decode('utf-8'))

        if 'error' in response:
            raise Exception(f"MCP Error: {response['error']['message']}")

        return response.get('result', {})

    def initialize(self) -> Dict:
        """Initialize MCP connection"""
        return self.send_request('initialize', {
            'protocolVersion': '2024-11-05',
            'capabilities': {'tools': {}},
            'clientInfo': {
                'name': 'erlvectordb-python-client',
                'version': '1.0.0'
            }
        })

    def list_tools(self) -> Dict:
        """List available tools"""
        return self.send_request('tools/list')

    def call_tool(self, name: str, arguments: Dict) -> Dict:
        """Call a specific tool"""
        return self.send_request('tools/call', {
            'name': name,
            'arguments': arguments
        })

    # Vector Database Operations
    def create_store(self, name: str) -> Dict:
        return self.call_tool('create_store', {'name': name})

    def insert_vector(self, store: str, vector_id: str, vector: List[float], 
                     metadata: Dict = None) -> Dict:
        return self.call_tool('insert_vector', {
            'store': store,
            'id': vector_id,
            'vector': vector,
            'metadata': metadata or {}
        })

    def search_vectors(self, store: str, vector: List[float], k: int = 10) -> Dict:
        return self.call_tool('search_vectors', {
            'store': store,
            'vector': vector,
            'k': k
        })

    def sync_store(self, store: str) -> Dict:
        return self.call_tool('sync_store', {'store': store})

    def backup_store(self, store: str, backup_name: str) -> Dict:
        return self.call_tool('backup_store', {
            'store': store,
            'backup_name': backup_name
        })

    def list_backups(self) -> Dict:
        return self.call_tool('list_backups', {})

    def disconnect(self):
        """Disconnect from server"""
        if self.socket:
            self.socket.close()
            self.socket = None

# Usage Example
if __name__ == '__main__':
    client = ErlVectorDBClient()
    
    try:
        client.connect()
        client.initialize()
        
        # List available tools
        tools = client.list_tools()
        print('Available tools:', tools)
        
        # Create a store
        client.create_store('python_test_store')
        
        # Insert vectors
        client.insert_vector('python_test_store', 'doc1', [1.0, 2.0, 3.0], 
                           {'title': 'Python Test Document'})
        
        # Search
        results = client.search_vectors('python_test_store', [1.1, 2.1, 3.1], 5)
        print('Search results:', results)
        
    except Exception as e:
        print(f'Error: {e}')
    finally:
        client.disconnect()
```

### Available Tools (by scope):

**Read scope:**
- `search_vectors`: Search for similar vectors

**Write scope:**
- `create_store`: Create a new vector store
- `insert_vector`: Insert a vector with metadata
- `sync_store`: Sync a store to persistent storage

**Admin scope:**
- `backup_store`: Create a backup of a store
- `restore_store`: Restore a store from backup
- `list_backups`: List all available backups

### MCP Client Example with OAuth

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "insert_vector",
    "arguments": {
      "store": "my_store",
      "id": "doc1",
      "vector": [1.0, 2.0, 3.0],
      "metadata": {"title": "Document 1"}
    }
  },
  "auth": {
    "type": "bearer",
    "token": "your_access_token_here"
  },
  "id": 1
}
```

### MCP Connection Testing

#### Test OAuth Connection
```bash
# Test OAuth token endpoint
curl -X POST http://localhost:8081/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write admin"
```

#### Test MCP Connection
```bash
# Test MCP server connectivity
telnet localhost 8080

# Send initialize request (after connecting)
{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}}},"id":1}
```

### MCP Integration Patterns

#### Streaming Responses
For large result sets, implement streaming:

```javascript
// Handle streaming responses
client.socket.on('data', (chunk) => {
    const lines = chunk.toString().split('\n');
    lines.forEach(line => {
        if (line.trim()) {
            try {
                const response = JSON.parse(line);
                handleStreamingResponse(response);
            } catch (e) {
                // Handle partial JSON
                buffer += line;
            }
        }
    });
});
```

#### Connection Pooling
For high-throughput applications:

```javascript
class ErlVectorDBPool {
    constructor(options, poolSize = 5) {
        this.options = options;
        this.poolSize = poolSize;
        this.connections = [];
        this.available = [];
    }

    async getConnection() {
        if (this.available.length > 0) {
            return this.available.pop();
        }
        
        if (this.connections.length < this.poolSize) {
            const client = new ErlVectorDBClient(this.options);
            await client.connect();
            await client.initialize();
            this.connections.push(client);
            return client;
        }
        
        // Wait for available connection
        return new Promise(resolve => {
            const checkAvailable = () => {
                if (this.available.length > 0) {
                    resolve(this.available.pop());
                } else {
                    setTimeout(checkAvailable, 10);
                }
            };
            checkAvailable();
        });
    }

    releaseConnection(client) {
        this.available.push(client);
    }
}
```

#### Error Handling and Retry Logic
```javascript
class RobustErlVectorDBClient extends ErlVectorDBClient {
    async sendRequestWithRetry(method, params, maxRetries = 3) {
        for (let attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                return await this.sendRequest(method, params);
            } catch (error) {
                if (attempt === maxRetries) throw error;
                
                // Handle specific error types
                if (error.message.includes('token_expired')) {
                    await this.getAccessToken();
                } else if (error.message.includes('connection')) {
                    await this.connect();
                    await this.initialize();
                }
                
                // Exponential backoff
                await new Promise(resolve => 
                    setTimeout(resolve, Math.pow(2, attempt) * 1000)
                );
            }
        }
    }
}
```

### MCP Troubleshooting

#### Common Issues

**1. Authentication Errors**
```
Error: Authentication required
```
- Verify OAuth server is running on port 8081
- Check client credentials are correct
- Ensure token hasn't expired (default: 1 hour)

**2. Connection Refused**
```
Error: ECONNREFUSED
```
- Verify ErlVectorDB is running
- Check MCP port configuration (default: 8080)
- Ensure firewall allows connections

**3. Invalid Tool Calls**
```
Error: Insufficient permissions
```
- Check OAuth scopes match tool requirements
- Verify client has necessary permissions
- Review scope-based access control

**4. JSON Parse Errors**
```
Error: Unexpected token in JSON
```
- Ensure proper JSON formatting
- Check for trailing commas or syntax errors
- Verify UTF-8 encoding

#### Debug Mode

Enable debug logging in ErlVectorDB:
```erlang
% In config/sys.config
{kernel, [
    {logger_level, debug},
    {logger, [
        {handler, default, logger_std_h,
            #{config => #{type => standard_io},
              formatter => {logger_formatter, #{}}}}
    ]}
]}
```

#### Health Check Endpoint

Test server health:
```bash
# Check if servers are responding
curl -f http://localhost:8081/oauth/client_info \
  -H "Authorization: Bearer YOUR_TOKEN" || echo "OAuth server down"

telnet localhost 8080 || echo "MCP server down"
```

### MCP Performance Optimization

#### Batch Operations
```javascript
// Batch insert multiple vectors
async function batchInsert(client, store, vectors) {
    const promises = vectors.map(({id, vector, metadata}) =>
        client.insertVector(store, id, vector, metadata)
    );
    return Promise.all(promises);
}
```

#### Connection Keep-Alive
```javascript
// Implement heartbeat to keep connection alive
setInterval(() => {
    if (client.socket && !client.socket.destroyed) {
        client.listTools().catch(err => {
            console.log('Heartbeat failed, reconnecting...');
            client.connect();
        });
    }
}, 30000); // 30 seconds
```

#### Compression for Large Vectors
```javascript
// Use compression for large vectors
async function insertLargeVector(client, store, id, vector, metadata) {
    if (vector.length > 1000) {
        // Let server handle compression
        return client.callTool('insert_compressed_vector', {
            store, id, vector, metadata
        });
    } else {
        return client.insertVector(store, id, vector, metadata);
    }
}
```

## Configuration

Edit `config/sys.config`:

```erlang
[
    {erlvectordb, [
        % Network configuration
        {mcp_port, 8080},
        {oauth_port, 8081},
        {rest_api_port, 8082},
        {rest_api_enabled, true},
        
        % Authentication
        {oauth_enabled, true},
        {create_default_client, true},
        {default_client_id, <<"admin">>},
        {default_client_secret, <<"admin_secret_2024">>},
        {token_lifetime, 3600000},  % 1 hour
        {refresh_token_lifetime, 86400000},  % 24 hours
        
        % Clustering
        {cluster_enabled, false},
        {node_name, 'erlvectordb@localhost'},
        {cluster_cookie, erlvectordb_cluster},
        {replication_factor, 2},
        {heartbeat_interval, 5000},
        
        % Compression
        {compression_enabled, true},
        {compression_algorithm, quantization_8bit},
        
        % Storage
        {persistence_enabled, true},
        {persistence_dir, "data"},
        {backup_dir, "backups"},
        {sync_interval, 30000}
    ]}
].
```

## Testing

```bash
rebar3 ct
```

## Performance Characteristics

- **Concurrent Operations**: Each vector store runs in its own process
- **Parallel Search**: Search operations can run concurrently across stores and nodes
- **Memory Efficient**: Leverages Erlang's copy-on-write semantics + compression
- **Fault Tolerant**: Individual store crashes don't affect others
- **Horizontal Scaling**: Linear scaling with cluster nodes
- **Compression Benefits**: 50-90% storage reduction with configurable accuracy trade-offs
- **Network Optimization**: Compressed replication reduces bandwidth usage

## Persistence & Backup

### Automatic Persistence

All vector operations are automatically persisted to disk using DETS (Disk Erlang Term Storage) with ETS for fast in-memory access:

```erlang
% Data is automatically saved, but you can force sync
erlvectordb:sync(my_store).
```

### Backup Operations

```erlang
% Create a backup
{ok, BackupInfo} = erlvectordb:backup_store(my_store, "production_backup"),
BackupPath = BackupInfo#backup_info.file_path.

% List all backups
{ok, Backups} = erlvectordb:list_backups().

% Restore from backup to new store
{ok, Result} = erlvectordb:restore_store(BackupPath, new_store_name).
```

### Export/Import

```erlang
% Export to JSON format
{ok, _} = erlvectordb:export_store(my_store, "export.json").

% Import from JSON
{ok, _} = erlvectordb:import_store("export.json", imported_store).
```

## Advanced Features

### Custom Distance Metrics

```erlang
% Use vector_utils for custom operations
Similarity = vector_utils:cosine_similarity(Vector1, Vector2),
Distance = vector_utils:euclidean_distance(Vector1, Vector2).
```

### Index Management

```erlang
% Create advanced indexes (future feature)
vector_index_manager:create_index(my_index, #{type => hnsw, m => 16}).
```

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `rebar3 ct`
5. Submit a pull request

## Roadmap

- [x] Persistent storage with DETS/ETS
- [x] Backup and recovery system
- [x] JSON export/import functionality
- [x] OAuth 2.1 authentication with scope-based permissions
- [x] Distributed clustering with automatic replication
- [x] REST API alongside MCP protocol
- [x] Vector compression with multiple algorithms
- [ ] HNSW indexing for approximate nearest neighbor search
- [ ] Batch operations for improved throughput
- [ ] Vector quantization optimization
- [ ] Prometheus metrics integration
- [ ] Automatic backup scheduling
- [ ] Advanced compression algorithms (LSH, Random Projection)
- [ ] JWT token support
- [ ] PKCE flow for public clients
- [ ] GraphQL API
- [ ] WebSocket real-time updates

## Architecture Benefits

This implementation leverages Erlang/OTP's unique strengths:

1. **Fault Isolation**: Each vector store is isolated in its own process
2. **Hot Code Reloading**: Update algorithms without stopping the database
3. **Massive Concurrency**: Handle thousands of concurrent operations
4. **Distribution**: Native clustering across multiple nodes with automatic failover
5. **Supervision**: Automatic restart of failed components
6. **Pattern Matching**: Efficient message routing and data processing
7. **Persistent Storage**: DETS provides durability with ETS performance
8. **Backup System**: Built-in backup/restore with JSON interoperability
9. **OAuth 2.1 Security**: Industry-standard authentication with scope-based access control
10. **Compression**: Intelligent vector compression reduces storage by 50-90%
11. **REST Integration**: Full REST API for web applications alongside MCP protocol
