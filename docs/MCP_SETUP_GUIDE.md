# ErlVectorDB MCP Setup Guide

This guide walks you through setting up and connecting to the ErlVectorDB MCP (Model Context Protocol) server.

## Quick Start

### 1. Start ErlVectorDB

```bash
# Clone and build
git clone <repository>
cd erlvectordb
rebar3 compile

# Start the application
rebar3 shell
```

### 2. Verify Services are Running

```bash
# Check MCP server (should connect)
telnet localhost 8080

# Check OAuth server (should return JSON)
curl http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read"
```

### 3. Test with Example Client

```bash
# Node.js example
cd examples
node mcp_client.js

# Python example  
python mcp_client.py
```

## Configuration

### Server Configuration

Edit `config/sys.config`:

```erlang
[
    {erlvectordb, [
        % MCP Server
        {mcp_port, 8080},
        
        % OAuth Server  
        {oauth_port, 8081},
        {oauth_enabled, true},
        
        % Default OAuth Client
        {create_default_client, true},
        {default_client_id, <<"admin">>},
        {default_client_secret, <<"admin_secret_2024">>},
        
        % Token Lifetimes
        {token_lifetime, 3600000},        % 1 hour
        {refresh_token_lifetime, 86400000} % 24 hours
    ]}
].
```

### Client Configuration

#### Environment Variables

```bash
export ERLVECTORDB_HOST=localhost
export ERLVECTORDB_PORT=8080
export ERLVECTORDB_OAUTH_HOST=localhost
export ERLVECTORDB_OAUTH_PORT=8081
export ERLVECTORDB_CLIENT_ID=admin
export ERLVECTORDB_CLIENT_SECRET=admin_secret_2024
```

#### Claude Desktop Configuration

Add to `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "node",
      "args": ["./examples/mcp_client.js"],
      "env": {
        "ERLVECTORDB_HOST": "localhost",
        "ERLVECTORDB_PORT": "8080",
        "ERLVECTORDB_CLIENT_ID": "admin",
        "ERLVECTORDB_CLIENT_SECRET": "admin_secret_2024"
      }
    }
  }
}
```

## Authentication

### OAuth 2.1 Flow

1. **Get Access Token**
   ```bash
   curl -X POST http://localhost:8081/oauth/token \
     -H "Content-Type: application/x-www-form-urlencoded" \
     -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024&scope=read write admin"
   ```

2. **Use Token in MCP Requests**
   ```json
   {
     "jsonrpc": "2.0",
     "method": "tools/list",
     "auth": {
       "type": "bearer", 
       "token": "YOUR_ACCESS_TOKEN"
     },
     "id": 1
   }
   ```

### Scopes and Permissions

- **`read`**: Search vectors, get statistics
- **`write`**: Insert/delete vectors, create stores, sync
- **`admin`**: Backup/restore, client management

### Creating Custom OAuth Clients

```erlang
% In Erlang shell
erlvectordb:register_oauth_client(<<"my_app">>, <<"secure_secret">>, #{
    scopes => [<<"read">>, <<"write">>],
    grant_types => [<<"client_credentials">>, <<"refresh_token">>]
}).
```

## MCP Protocol

### Available Tools

| Tool | Scope | Description |
|------|-------|-------------|
| `create_store` | write | Create a new vector store |
| `insert_vector` | write | Insert a vector with metadata |
| `search_vectors` | read | Search for similar vectors |
| `sync_store` | write | Sync store to persistent storage |
| `backup_store` | admin | Create a backup |
| `restore_store` | admin | Restore from backup |
| `list_backups` | admin | List all backups |

### Request Format

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "TOOL_NAME",
    "arguments": {
      // Tool-specific arguments
    }
  },
  "auth": {
    "type": "bearer",
    "token": "ACCESS_TOKEN"
  },
  "id": 1
}
```

### Response Format

```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Result data"
      }
    ]
  },
  "id": 1
}
```

## Troubleshooting

### Common Issues

**Connection Refused**
```
Error: ECONNREFUSED localhost:8080
```
- Check if ErlVectorDB is running: `rebar3 shell`
- Verify port configuration in `config/sys.config`
- Check firewall settings

**Authentication Failed**
```
Error: Authentication required
```
- Verify OAuth server is running on port 8081
- Check client credentials are correct
- Ensure token hasn't expired

**Tool Permission Denied**
```
Error: Insufficient permissions
```
- Check OAuth scopes match tool requirements
- Verify client has necessary permissions
- Review scope-based access control

**JSON Parse Errors**
```
Error: Unexpected token in JSON
```
- Ensure proper JSON formatting
- Check for trailing commas
- Verify UTF-8 encoding

### Debug Mode

Enable debug logging:

```erlang
% In config/sys.config
{kernel, [
    {logger_level, debug}
]}
```

### Health Checks

```bash
# Test OAuth endpoint
curl -f http://localhost:8081/oauth/client_info \
  -H "Authorization: Bearer TOKEN" || echo "OAuth server down"

# Test MCP endpoint  
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | nc localhost 8080
```

## Performance Tips

### Connection Pooling

For high-throughput applications, implement connection pooling:

```javascript
class MCPConnectionPool {
    constructor(options, poolSize = 5) {
        this.options = options;
        this.poolSize = poolSize;
        this.available = [];
        this.total = 0;
    }
    
    async getConnection() {
        if (this.available.length > 0) {
            return this.available.pop();
        }
        
        if (this.total < this.poolSize) {
            const client = new ErlVectorDBClient(this.options);
            await client.connect();
            await client.initialize();
            this.total++;
            return client;
        }
        
        // Wait for available connection
        return new Promise(resolve => {
            const check = () => {
                if (this.available.length > 0) {
                    resolve(this.available.pop());
                } else {
                    setTimeout(check, 10);
                }
            };
            check();
        });
    }
    
    releaseConnection(client) {
        this.available.push(client);
    }
}
```

### Batch Operations

```javascript
// Batch insert multiple vectors
async function batchInsert(client, store, vectors) {
    const promises = vectors.map(({id, vector, metadata}) =>
        client.insertVector(store, id, vector, metadata)
    );
    return Promise.all(promises);
}
```

### Keep-Alive

```javascript
// Implement heartbeat
setInterval(async () => {
    try {
        await client.listTools();
    } catch (error) {
        console.log('Heartbeat failed, reconnecting...');
        await client.connect();
        await client.initialize();
    }
}, 30000);
```

## Security Best Practices

1. **Use Strong Secrets**: Change default client secret in production
2. **Limit Scopes**: Only grant necessary permissions
3. **Token Rotation**: Implement refresh token rotation
4. **Network Security**: Use TLS in production (configure reverse proxy)
5. **Access Control**: Restrict network access to MCP ports
6. **Monitoring**: Log authentication attempts and failures

## Production Deployment

### Reverse Proxy Configuration (nginx)

```nginx
upstream erlvectordb_mcp {
    server localhost:8080;
}

upstream erlvectordb_oauth {
    server localhost:8081;
}

server {
    listen 443 ssl;
    server_name vectordb.example.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    location /mcp {
        proxy_pass http://erlvectordb_mcp;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
    
    location /oauth {
        proxy_pass http://erlvectordb_oauth;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Systemd Service

```ini
[Unit]
Description=ErlVectorDB MCP Server
After=network.target

[Service]
Type=forking
User=erlvectordb
Group=erlvectordb
WorkingDirectory=/opt/erlvectordb
ExecStart=/opt/erlvectordb/bin/erlvectordb start
ExecStop=/opt/erlvectordb/bin/erlvectordb stop
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

## Support

For issues and questions:

1. Check this guide and troubleshooting section
2. Review example clients in `examples/` directory
3. Enable debug logging for detailed error information
4. Check server logs for authentication and connection issues

## AI Integration with Gemini

### Gemini AI MCP Client

For AI-powered semantic search and document analysis, use the Gemini integration:

```bash
# Setup Gemini integration
export GEMINI_API_KEY='your-gemini-api-key'
pip install google-generativeai
bash examples/setup_gemini_demo.sh

# Run AI-enhanced demo
python examples/gemini_mcp_client.py
```

**Features:**
- **AI-Generated Embeddings**: Use Gemini to create semantic vector representations
- **Smart Document Analysis**: Automatic categorization, sentiment analysis, and keyword extraction
- **Semantic Search**: Natural language queries with AI-powered explanations
- **Intelligent Metadata**: Auto-generated document metadata using AI

**Example Usage:**
```python
from examples.gemini_mcp_client import GeminiMCPClient

client = GeminiMCPClient(gemini_api_key="your-key")
client.connect_to_vectordb()

# AI-powered document insertion
result = client.smart_insert('ai_store', 'doc1', 
    'Artificial intelligence is transforming healthcare...')

# Semantic search with explanation
search = client.smart_search('medical AI applications', 'ai_store')
print(search['explanation'])  # AI explains why results are relevant
```

**Claude Desktop Integration:**
```json
{
  "mcpServers": {
    "erlvectordb-gemini": {
      "command": "python3",
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

Get your Gemini API key from: https://makersuite.google.com/app/apikey