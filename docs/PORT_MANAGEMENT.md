# Port Management Configuration Guide

This guide explains how to configure the ErlVectorDB port management system for different deployment scenarios.

## Overview

The port management system provides:
- Automatic port conflict detection and resolution
- Configurable port ranges for each service
- Development mode with automatic port selection
- Container deployment support with health checks
- Graceful startup sequencing and error handling

## Configuration Sources

Port configuration is loaded from multiple sources in priority order:

1. **Environment Variables** (highest priority)
2. **Configuration Files**
3. **Application Environment**
4. **Default Configuration** (lowest priority)

## Basic Configuration

### Application Environment

Add to your `sys.config` or application environment:

```erlang
{erlvectordb, [
    {port_config, #{
        mcp_server => #{
            preferred_port => 8080,
            port_range => {8080, 8090},
            bind_interface => "127.0.0.1",
            required => true,
            startup_order => 1,
            health_check_path => "/health"
        },
        oauth_server => #{
            preferred_port => 8081,
            port_range => {8081, 8091},
            bind_interface => "127.0.0.1",
            required => true,
            startup_order => 2,
            health_check_path => "/oauth/health"
        },
        rest_api_server => #{
            preferred_port => 8082,
            port_range => {8082, 8092},
            bind_interface => "127.0.0.1",
            required => true,
            startup_order => 3,
            health_check_path => "/api/health"
        }
    }}
]}
```

### Configuration Options

Each service supports these configuration options:

- `preferred_port`: The preferred port number to bind to
- `port_range`: Tuple `{StartPort, EndPort}` defining the range for fallback ports
- `bind_interface`: Interface to bind to ("127.0.0.1" for localhost, "0.0.0.0" for all interfaces)
- `required`: Boolean indicating if the service is required for startup
- `startup_order`: Integer defining the order in which services start
- `health_check_path`: HTTP path for health checks (container deployments)

## Development Mode

### Enabling Development Mode

Set development mode via:

1. **Application Environment:**
   ```erlang
   {development_mode, true}
   ```

2. **Environment Variable:**
   ```bash
   export ERLVECTORDB_DEV_MODE=true
   # or
   export NODE_ENV=development
   ```

### Development Configuration

```erlang
{development_config, #{
    base_ports => #{
        mcp_server => 9080,
        oauth_server => 9081,
        rest_api_server => 9082
    },
    timeout_ms => 2000,        % Shorter timeout for development
    port_range_size => 20      % Larger port range
}}
```

### Development Features

- Automatic port selection starting from base ports
- Shorter binding timeouts (2 seconds vs 5 seconds)
- Force restart functionality to kill existing instances
- Enhanced logging for debugging

### Development Commands

```bash
# Force restart all services (development mode only)
./dev-cli.sh force-restart

# Kill existing instances before starting
./dev-cli.sh start --force
```

## Container Deployment

### Container Mode Detection

Container mode is automatically detected based on:
- `CONTAINER=true` environment variable
- `DOCKER=true` environment variable
- Presence of `KUBERNETES_SERVICE_HOST`
- Container-like hostname patterns

### Container Configuration

Use `config/container.config` for container deployments:

```erlang
{container_mode, true},
{port_config, #{
    mcp_server => #{
        preferred_port => 8080,
        port_range => {8080, 8090},
        bind_interface => "0.0.0.0",  % Bind to all interfaces
        required => true,
        startup_order => 1,
        health_check_path => "/health",
        container_mode => true
    }
    % ... other services
}},
{container_config, #{
    bind_interface => "0.0.0.0",
    health_check_enabled => true,
    graceful_shutdown_timeout => 30000,
    port_mapping_logging => true
}}
```

### Environment Variables for Containers

#### Service-Specific Ports
```bash
# Set specific ports for each service
MCP_SERVER_PORT=8080
OAUTH_SERVER_PORT=8081
REST_API_SERVER_PORT=8082

# Set port ranges
MCP_SERVER_PORT_RANGE_START=8080
MCP_SERVER_PORT_RANGE_END=8090

# Set bind interfaces
MCP_SERVER_BIND_INTERFACE=0.0.0.0
OAUTH_SERVER_BIND_INTERFACE=0.0.0.0
REST_API_SERVER_BIND_INTERFACE=0.0.0.0
```

#### Generic Container Variables
```bash
# Generic port (used if service-specific not set)
PORT=8080

# Bind to all interfaces
BIND_ALL_INTERFACES=true

# Enable port mapping logs
LOG_PORT_MAPPINGS=true

# Graceful shutdown timeout (seconds)
GRACEFUL_SHUTDOWN_TIMEOUT=30
```

### Docker Example

```dockerfile
FROM erlang:25-alpine

# Copy application
COPY . /app
WORKDIR /app

# Set container mode
ENV CONTAINER=true
ENV BIND_ALL_INTERFACES=true
ENV LOG_PORT_MAPPINGS=true

# Expose ports
EXPOSE 8080 8081 8082

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

# Start application
CMD ["./start.sh", "--config", "config/container.config"]
```

### Docker Compose Example

```yaml
version: '3.8'
services:
  erlvectordb:
    build: .
    ports:
      - "8080:8080"  # MCP Server
      - "8081:8081"  # OAuth Server
      - "8082:8082"  # REST API Server
    environment:
      - CONTAINER=true
      - BIND_ALL_INTERFACES=true
      - LOG_PORT_MAPPINGS=true
      - MCP_SERVER_EXTERNAL_PORT=8080
      - OAUTH_SERVER_EXTERNAL_PORT=8081
      - REST_API_SERVER_EXTERNAL_PORT=8082
    volumes:
      - ./data:/data
      - ./backups:/backups
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

### Kubernetes Example

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlvectordb
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlvectordb
  template:
    metadata:
      labels:
        app: erlvectordb
    spec:
      containers:
      - name: erlvectordb
        image: erlvectordb:latest
        ports:
        - containerPort: 8080
          name: mcp
        - containerPort: 8081
          name: oauth
        - containerPort: 8082
          name: api
        env:
        - name: CONTAINER
          value: "true"
        - name: BIND_ALL_INTERFACES
          value: "true"
        - name: LOG_PORT_MAPPINGS
          value: "true"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        volumeMounts:
        - name: data
          mountPath: /data
        - name: backups
          mountPath: /backups
      volumes:
      - name: data
        persistentVolumeClaim:
          claimName: erlvectordb-data
      - name: backups
        persistentVolumeClaim:
          claimName: erlvectordb-backups
---
apiVersion: v1
kind: Service
metadata:
  name: erlvectordb-service
spec:
  selector:
    app: erlvectordb
  ports:
  - name: mcp
    port: 8080
    targetPort: 8080
  - name: oauth
    port: 8081
    targetPort: 8081
  - name: api
    port: 8082
    targetPort: 8082
  type: LoadBalancer
```

## Environment Variable Reference

### Service Configuration
| Variable | Description | Example |
|----------|-------------|---------|
| `{SERVICE}_PORT` | Preferred port for service | `MCP_SERVER_PORT=8080` |
| `{SERVICE}_PORT_RANGE_START` | Start of port range | `MCP_SERVER_PORT_RANGE_START=8080` |
| `{SERVICE}_PORT_RANGE_END` | End of port range | `MCP_SERVER_PORT_RANGE_END=8090` |
| `{SERVICE}_BIND_INTERFACE` | Bind interface | `MCP_SERVER_BIND_INTERFACE=0.0.0.0` |
| `{SERVICE}_REQUIRED` | Required flag | `MCP_SERVER_REQUIRED=true` |
| `{SERVICE}_HEALTH_PATH` | Health check path | `MCP_SERVER_HEALTH_PATH=/health` |

### Development Mode
| Variable | Description | Example |
|----------|-------------|---------|
| `ERLVECTORDB_DEV_MODE` | Enable development mode | `ERLVECTORDB_DEV_MODE=true` |
| `NODE_ENV` | Node environment | `NODE_ENV=development` |
| `ERLVECTORDB_DEV_TIMEOUT_MS` | Development timeout | `ERLVECTORDB_DEV_TIMEOUT_MS=2000` |
| `ERLVECTORDB_DEV_{SERVICE}_BASE_PORT` | Development base port | `ERLVECTORDB_DEV_MCP_SERVER_BASE_PORT=9080` |

### Container Mode
| Variable | Description | Example |
|----------|-------------|---------|
| `CONTAINER` | Enable container mode | `CONTAINER=true` |
| `DOCKER` | Docker indicator | `DOCKER=true` |
| `BIND_ALL_INTERFACES` | Bind to all interfaces | `BIND_ALL_INTERFACES=true` |
| `LOG_PORT_MAPPINGS` | Log port mappings | `LOG_PORT_MAPPINGS=true` |
| `GRACEFUL_SHUTDOWN_TIMEOUT` | Shutdown timeout (seconds) | `GRACEFUL_SHUTDOWN_TIMEOUT=30` |
| `{SERVICE}_EXTERNAL_PORT` | External port mapping | `MCP_SERVER_EXTERNAL_PORT=8080` |

## API Reference

### Port Status API

Query current port allocations:

```bash
curl http://localhost:8082/api/ports/status
```

Response:
```json
{
  "status": "ok",
  "ports": [
    {
      "service": "mcp_server",
      "port": 8080,
      "status": "bound",
      "allocated_at": 1640995200,
      "bind_attempts": 1
    },
    {
      "service": "oauth_server", 
      "port": 8081,
      "status": "bound",
      "allocated_at": 1640995201,
      "bind_attempts": 1
    }
  ]
}
```

### Health Check Endpoints

Each service provides a health check endpoint:

- MCP Server: `GET /health`
- OAuth Server: `GET /oauth/health`
- REST API Server: `GET /api/health`

## Troubleshooting

### Common Issues

1. **Port Already in Use**
   ```
   Error: Port 8080 already in use
   Solution: Check port_range configuration or kill conflicting process
   ```

2. **No Ports Available in Range**
   ```
   Error: No ports available in range {8080, 8090}
   Solution: Expand port range or free up ports in the range
   ```

3. **Service Startup Failed**
   ```
   Error: Service startup failed: oauth_server
   Solution: Check service dependencies and port allocation
   ```

### Debugging

Enable debug logging:
```erlang
{kernel, [
    {logger_level, debug}
]}
```

Check port allocations:
```bash
# Via API
curl http://localhost:8082/api/ports/status

# Via system tools
netstat -tulpn | grep :808
lsof -i :8080
```

### Development Mode Debugging

```bash
# Check development mode status
./dev-cli.sh status

# Force restart with debug logging
./dev-cli.sh force-restart --debug

# Kill existing instances manually
./dev-cli.sh kill-instances
```

## Migration Guide

### From Legacy Configuration

Old format:
```erlang
{mcp_port, 8080},
{oauth_port, 8081},
{rest_api_port, 8082}
```

New format:
```erlang
{port_config, #{
    mcp_server => #{preferred_port => 8080},
    oauth_server => #{preferred_port => 8081},
    rest_api_server => #{preferred_port => 8082}
}}
```

The system supports both formats for backward compatibility.

### Upgrading to Container Mode

1. Update configuration to use `0.0.0.0` bind interface
2. Set `container_mode => true`
3. Configure health check endpoints
4. Set appropriate graceful shutdown timeout
5. Test port mapping logging

## Best Practices

1. **Use port ranges** to allow automatic fallback
2. **Set appropriate bind interfaces** (127.0.0.1 for local, 0.0.0.0 for containers)
3. **Configure health checks** for container orchestration
4. **Use development mode** for local development
5. **Monitor port allocations** via the status API
6. **Set graceful shutdown timeouts** for production deployments
7. **Use environment variables** for container-specific overrides