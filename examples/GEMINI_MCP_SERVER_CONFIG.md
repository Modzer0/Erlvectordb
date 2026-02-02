# Gemini MCP Server Configuration Guide

This document provides comprehensive configuration guidance for the `gemini_mcp_server.py` script, which acts as a bridge between Gemini CLI and ErlVectorDB.

## Overview

The Gemini MCP Server is a Python script that:
- Communicates with Gemini CLI via stdio (standard input/output)
- Forwards requests to ErlVectorDB MCP server via TCP sockets
- Handles OAuth 2.1 authentication automatically
- Provides robust error handling and automatic reconnection
- Supports configurable timeouts, retries, and buffer sizes

## Architecture

```
┌─────────────┐         ┌──────────────────┐         ┌──────────────┐
│  Gemini CLI │◄──stdio─┤ gemini_mcp_server│◄──TCP───┤ ErlVectorDB  │
│             │         │     (Bridge)      │         │  MCP Server  │
└─────────────┘         └──────────────────┘         └──────────────┘
                               │                             │
                               │         ┌──────────────┐    │
                               └──HTTP───┤ OAuth Server │────┘
                                         └──────────────┘
```

## Configuration Methods

### 1. Environment Variables (Recommended)

Set environment variables before running the server:

```bash
export ERLVECTORDB_HOST=localhost
export ERLVECTORDB_PORT=8080
python examples/gemini_mcp_server.py
```

### 2. Gemini CLI Configuration File

Configure in `~/.config/gemini-cli/config.json`:

```json
{
  "mcpServers": {
    "erlvectordb": {
      "command": "python",
      "args": ["/path/to/gemini_mcp_server.py"],
      "env": {
        "ERLVECTORDB_HOST": "localhost",
        "ERLVECTORDB_PORT": "8080",
        "ERLVECTORDB_LOG_LEVEL": "INFO"
      }
    }
  }
}
```

### 3. Programmatic Configuration

Use `ServerConfig` class in Python code:

```python
from examples.gemini_mcp_server import ServerConfig, MCPServer

config = ServerConfig(
    erlvectordb_host='localhost',
    erlvectordb_port=8080,
    log_level='DEBUG'
)
config.validate()

server = MCPServer(config)
server.run()
```

## Configuration Options

### Connection Settings

#### `ERLVECTORDB_HOST`
- **Type**: String
- **Default**: `localhost`
- **Description**: Hostname or IP address of the ErlVectorDB MCP server
- **Example**: `export ERLVECTORDB_HOST=192.168.1.100`

#### `ERLVECTORDB_PORT`
- **Type**: Integer (1-65535)
- **Default**: `8080`
- **Description**: Port number of the ErlVectorDB MCP server
- **Example**: `export ERLVECTORDB_PORT=8080`

#### `ERLVECTORDB_OAUTH_HOST`
- **Type**: String
- **Default**: `localhost`
- **Description**: Hostname or IP address of the OAuth server
- **Example**: `export ERLVECTORDB_OAUTH_HOST=localhost`

#### `ERLVECTORDB_OAUTH_PORT`
- **Type**: Integer (1-65535)
- **Default**: `8081`
- **Description**: Port number of the OAuth server
- **Example**: `export ERLVECTORDB_OAUTH_PORT=8081`

### Authentication Settings

#### `ERLVECTORDB_CLIENT_ID`
- **Type**: String
- **Default**: `admin`
- **Description**: OAuth client ID for authentication
- **Security**: Keep this value secure, do not commit to version control
- **Example**: `export ERLVECTORDB_CLIENT_ID=my_client`

#### `ERLVECTORDB_CLIENT_SECRET`
- **Type**: String
- **Default**: `admin_secret_2024`
- **Description**: OAuth client secret for authentication
- **Security**: Keep this value secure, do not commit to version control
- **Example**: `export ERLVECTORDB_CLIENT_SECRET=my_secure_secret`

### Socket Configuration

#### `ERLVECTORDB_SOCKET_TIMEOUT`
- **Type**: Integer (seconds)
- **Default**: `30`
- **Description**: Socket timeout for send/receive operations
- **Recommendation**: Increase for high-latency networks or large messages
- **Example**: `export ERLVECTORDB_SOCKET_TIMEOUT=60`

#### `ERLVECTORDB_BUFFER_SIZE`
- **Type**: Integer (bytes)
- **Default**: `8192` (8 KB)
- **Description**: Initial buffer size for reading socket data
- **Recommendation**: Increase for large vector operations
- **Example**: `export ERLVECTORDB_BUFFER_SIZE=32768`

### Reconnection Settings

#### `ERLVECTORDB_MAX_RECONNECT_ATTEMPTS`
- **Type**: Integer
- **Default**: `3`
- **Description**: Maximum number of reconnection attempts after connection loss
- **Recommendation**: Increase for unreliable networks
- **Example**: `export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=5`

#### `ERLVECTORDB_RECONNECT_DELAY`
- **Type**: Float (seconds)
- **Default**: `1.0`
- **Description**: Initial delay between reconnection attempts (exponential backoff)
- **Example**: `export ERLVECTORDB_RECONNECT_DELAY=2.0`

### OAuth Retry Settings

#### `ERLVECTORDB_OAUTH_MAX_RETRIES`
- **Type**: Integer
- **Default**: `3`
- **Description**: Maximum number of OAuth token request retries
- **Example**: `export ERLVECTORDB_OAUTH_MAX_RETRIES=5`

#### `ERLVECTORDB_OAUTH_INITIAL_BACKOFF`
- **Type**: Float (seconds)
- **Default**: `1.0`
- **Description**: Initial backoff delay for OAuth retries
- **Example**: `export ERLVECTORDB_OAUTH_INITIAL_BACKOFF=2.0`

#### `ERLVECTORDB_OAUTH_MAX_BACKOFF`
- **Type**: Float (seconds)
- **Default**: `30.0`
- **Description**: Maximum backoff delay for OAuth retries
- **Example**: `export ERLVECTORDB_OAUTH_MAX_BACKOFF=60.0`

#### `ERLVECTORDB_OAUTH_BACKOFF_MULTIPLIER`
- **Type**: Float
- **Default**: `2.0`
- **Description**: Multiplier for exponential backoff (delay *= multiplier)
- **Example**: `export ERLVECTORDB_OAUTH_BACKOFF_MULTIPLIER=1.5`

### Logging Settings

#### `ERLVECTORDB_LOG_LEVEL`
- **Type**: String
- **Default**: `INFO`
- **Valid Values**: `DEBUG`, `INFO`, `WARNING`, `ERROR`, `CRITICAL`
- **Description**: Logging level for stderr output
- **Recommendation**: Use `DEBUG` for troubleshooting, `INFO` for production
- **Example**: `export ERLVECTORDB_LOG_LEVEL=DEBUG`

## Configuration Profiles

### Development Profile

For local development with verbose logging:

```bash
export ERLVECTORDB_HOST=localhost
export ERLVECTORDB_PORT=8080
export ERLVECTORDB_OAUTH_HOST=localhost
export ERLVECTORDB_OAUTH_PORT=8081
export ERLVECTORDB_CLIENT_ID=admin
export ERLVECTORDB_CLIENT_SECRET=admin_secret_2024
export ERLVECTORDB_LOG_LEVEL=DEBUG
export ERLVECTORDB_SOCKET_TIMEOUT=30
```

### Production Profile

For production with optimized settings:

```bash
export ERLVECTORDB_HOST=erlvectordb.example.com
export ERLVECTORDB_PORT=8080
export ERLVECTORDB_OAUTH_HOST=oauth.example.com
export ERLVECTORDB_OAUTH_PORT=8081
export ERLVECTORDB_CLIENT_ID=${PROD_CLIENT_ID}
export ERLVECTORDB_CLIENT_SECRET=${PROD_CLIENT_SECRET}
export ERLVECTORDB_LOG_LEVEL=WARNING
export ERLVECTORDB_SOCKET_TIMEOUT=60
export ERLVECTORDB_BUFFER_SIZE=16384
export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=5
```

### High-Latency Network Profile

For networks with high latency or packet loss:

```bash
export ERLVECTORDB_SOCKET_TIMEOUT=120
export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=10
export ERLVECTORDB_RECONNECT_DELAY=5.0
export ERLVECTORDB_OAUTH_MAX_RETRIES=5
export ERLVECTORDB_OAUTH_MAX_BACKOFF=120.0
```

### Large Message Profile

For handling large vectors or bulk operations:

```bash
export ERLVECTORDB_BUFFER_SIZE=65536
export ERLVECTORDB_SOCKET_TIMEOUT=300
```

## Configuration Validation

The server validates all configuration values on startup. Invalid configurations will cause the server to exit with an error message.

### Validation Rules

- Port numbers must be between 1 and 65535
- Timeouts must be positive numbers
- Buffer size must be positive
- Reconnect attempts must be non-negative
- Log level must be one of: DEBUG, INFO, WARNING, ERROR, CRITICAL
- Required string fields (host, client_id, client_secret) cannot be empty

### Testing Configuration

Test your configuration before using with Gemini CLI:

```bash
# Test with echo
echo '{"jsonrpc":"2.0","method":"initialize","id":1,"params":{}}' | \
  python examples/gemini_mcp_server.py

# Expected output: JSON response with serverInfo
```

## Troubleshooting

### Configuration Not Applied

**Problem**: Environment variables don't seem to take effect

**Solution**: Ensure variables are exported before running the server:
```bash
export ERLVECTORDB_LOG_LEVEL=DEBUG  # Correct
ERLVECTORDB_LOG_LEVEL=DEBUG         # Wrong - not exported
```

### Connection Timeout

**Problem**: Server times out connecting to ErlVectorDB

**Solutions**:
1. Increase socket timeout: `export ERLVECTORDB_SOCKET_TIMEOUT=60`
2. Verify ErlVectorDB is running: `./check-status.sh`
3. Check network connectivity: `telnet localhost 8080`

### Authentication Failures

**Problem**: OAuth token requests fail

**Solutions**:
1. Verify OAuth server is running: `curl http://localhost:8081/oauth/token`
2. Check credentials are correct
3. Increase OAuth retry attempts: `export ERLVECTORDB_OAUTH_MAX_RETRIES=5`

### Large Message Truncation

**Problem**: Large vectors or responses are truncated

**Solutions**:
1. Increase buffer size: `export ERLVECTORDB_BUFFER_SIZE=32768`
2. Increase socket timeout: `export ERLVECTORDB_SOCKET_TIMEOUT=120`

### Debug Logging

Enable debug logging to diagnose issues:

```bash
export ERLVECTORDB_LOG_LEVEL=DEBUG
python examples/gemini_mcp_server.py 2> debug.log
```

## Security Considerations

### Credential Management

- Never commit `ERLVECTORDB_CLIENT_SECRET` to version control
- Use environment variables or secure secret management systems
- Rotate credentials regularly
- Use different credentials for development and production

### Network Security

- Use TLS/SSL for production deployments (requires proxy)
- Restrict network access to ErlVectorDB and OAuth servers
- Use firewall rules to limit connections
- Monitor for unauthorized access attempts

### Logging Security

- Logs are written to stderr only (stdout is for MCP protocol)
- Credentials are never logged
- Use appropriate log levels to avoid sensitive data exposure
- Secure log files with proper file permissions

## Advanced Configuration

### Custom Configuration Class

Create a custom configuration class for complex setups:

```python
from examples.gemini_mcp_server import ServerConfig, MCPServer
from dataclasses import dataclass

@dataclass
class CustomConfig(ServerConfig):
    custom_setting: str = 'default'
    
    @classmethod
    def from_custom_source(cls):
        # Load from database, config file, etc.
        return cls(
            erlvectordb_host='custom-host',
            custom_setting='custom-value'
        )

config = CustomConfig.from_custom_source()
server = MCPServer(config)
server.run()
```

### Configuration Inheritance

Override specific settings while keeping defaults:

```python
from examples.gemini_mcp_server import ServerConfig

# Start with environment defaults
config = ServerConfig.from_environment()

# Override specific settings
config.log_level = 'DEBUG'
config.socket_timeout = 120

# Validate and use
config.validate()
```

## Performance Tuning

### Optimal Settings for Different Scenarios

**Low-Latency Local Network:**
```bash
export ERLVECTORDB_SOCKET_TIMEOUT=10
export ERLVECTORDB_BUFFER_SIZE=8192
export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=2
```

**High-Throughput Operations:**
```bash
export ERLVECTORDB_BUFFER_SIZE=65536
export ERLVECTORDB_SOCKET_TIMEOUT=300
```

**Unreliable Network:**
```bash
export ERLVECTORDB_MAX_RECONNECT_ATTEMPTS=10
export ERLVECTORDB_RECONNECT_DELAY=5.0
export ERLVECTORDB_OAUTH_MAX_RETRIES=5
```

## References

- [ErlVectorDB Documentation](../README.md)
- [MCP Protocol Specification](https://modelcontextprotocol.io)
- [OAuth 2.1 Specification](https://oauth.net/2.1/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
