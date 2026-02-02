# ErlVectorDB Server Management Guide

Complete guide for managing your ErlVectorDB server instance.

## Quick Reference

| Command | Description |
|---------|-------------|
| `./start-local.sh` | Start the server with automatic setup |
| `./check-status.sh` | Check if server is running and healthy |
| `./stop-server.sh` | Stop server and cleanup all processes |
| `./test_server.sh` | Run full test suite |
| `rebar3 compile` | Compile the project |
| `rebar3 ct` | Run Common Test suites |

## Starting the Server

### Option 1: Using the Starter Script (Recommended)

```bash
./start-local.sh
```

**What it does:**
- ✅ Checks prerequisites (Erlang, rebar3)
- ✅ Creates necessary directories (data/, backups/, logs/)
- ✅ Checks port availability (8080, 8081, 8082)
- ✅ Optionally recompiles the project
- ✅ Optionally runs tests before starting
- ✅ Starts the server with logging

**Interactive prompts:**
- Recompile? (if already compiled)
- Run tests before starting?

### Option 2: Manual Start

```bash
# Compile first
rebar3 compile

# Start the server
rebar3 shell --eval "application:ensure_all_started(erlvectordb)"
```

### Option 3: Background Start

```bash
# Start in background
nohup rebar3 shell --eval "application:ensure_all_started(erlvectordb)" > logs/server.log 2>&1 &

# Save the PID
echo $! > .server.pid
```

## Checking Server Status

### Quick Status Check

```bash
./check-status.sh
```

**Output includes:**
- ✅ Process status (running/not running)
- ✅ Port availability (8080, 8081, 8082)
- ✅ OAuth endpoint test
- ✅ MCP endpoint test
- ✅ REST API endpoint test
- ✅ Server endpoints and credentials

### Manual Status Check

```bash
# Check if process is running
pgrep -f "rebar3 shell" || pgrep -f "beam.*erlvectordb"

# Check ports
lsof -i :8080 -i :8081 -i :8082

# Test OAuth endpoint
curl -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024"
```

## Stopping the Server

### Option 1: Using the Stop Script (Recommended)

```bash
./stop-server.sh
```

**What it does:**
- ✅ Stops all rebar3 shell processes
- ✅ Stops all Erlang VM processes
- ✅ Frees ports 8080, 8081, 8082
- ✅ Cleans up crash dumps
- ✅ Optionally stops epmd (Erlang Port Mapper Daemon)
- ✅ Optionally cleans up data directories
- ✅ Verifies cleanup was successful

**Interactive prompts:**
- Stop epmd? (if running)
- Clean up data directories?

### Option 2: Manual Stop

```bash
# Find and kill the process
pkill -f "rebar3 shell"

# Or kill by PID
kill $(pgrep -f "rebar3 shell")

# Force kill if needed
pkill -9 -f "rebar3 shell"

# If using background start with PID file
kill $(cat .server.pid)
rm .server.pid
```

### Option 3: From Erlang Shell

If you're in the Erlang shell:

```erlang
% Graceful shutdown
q().

% Or
init:stop().

% Force quit (Ctrl+C twice)
```

## Testing the Server

### Full Test Suite

```bash
./test_server.sh
```

**Tests:**
1. OAuth token generation
2. MCP server initialization
3. MCP tools list
4. REST API health check
5. OAuth client info

### Common Test Suites

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite test/oauth_SUITE
rebar3 ct --suite test/vector_store_SUITE
rebar3 ct --suite test/persistence_SUITE
rebar3 ct --suite test/clustering_SUITE
rebar3 ct --suite test/compression_SUITE
```

### Manual Testing

```bash
# Get OAuth token
TOKEN=$(curl -s -X POST http://localhost:8081/oauth/token \
  -d "grant_type=client_credentials&client_id=admin&client_secret=admin_secret_2024" | \
  jq -r '.access_token')

# Test REST API
curl -H "Authorization: Bearer $TOKEN" http://localhost:8082/health

# Test MCP (using netcat)
echo '{"jsonrpc":"2.0","method":"initialize","id":1}' | nc localhost 8080
```

## Troubleshooting

### Server Won't Start

**Problem:** Ports already in use

```bash
# Check what's using the ports
lsof -i :8080
lsof -i :8081
lsof -i :8082

# Stop the server properly
./stop-server.sh
```

**Problem:** Compilation errors

```bash
# Clean and rebuild
rebar3 clean
rebar3 compile
```

**Problem:** Missing dependencies

```bash
# Fetch dependencies
rebar3 get-deps
rebar3 compile
```

### Server Crashes

**Check crash dumps:**

```bash
# View Erlang crash dump
cat erl_crash.dump

# View rebar3 crash dump
cat rebar3.crashdump
```

**Check logs:**

```bash
# View startup logs
ls -lt logs/
tail -f logs/startup_*.log
```

### Port Conflicts

**Find what's using a port:**

```bash
# macOS/Linux
lsof -i :8080

# Alternative
netstat -an | grep 8080
```

**Kill process on port:**

```bash
# Kill by port
kill $(lsof -t -i:8080)

# Force kill
kill -9 $(lsof -t -i:8080)
```

### Memory Issues

**Check memory usage:**

```bash
# Find beam process
ps aux | grep beam

# Check memory
top -pid $(pgrep -f beam)
```

**Reduce memory usage:**

```erlang
% In config/sys.config
{erlvectordb, [
    {compression_enabled, true},  % Enable compression
    {sync_interval, 60000}        % Reduce sync frequency
]}
```

## Maintenance Tasks

### Backup Data

```bash
# Manual backup
cp -r data/ backups/data_$(date +%Y%m%d_%H%M%S)

# Or use Erlang API
rebar3 shell
> erlvectordb:backup_store(my_store, "daily_backup").
```

### Clean Up Old Logs

```bash
# Remove logs older than 7 days
find logs/ -name "*.log" -mtime +7 -delete

# Or remove all logs
rm -rf logs/*
```

### Update Configuration

```bash
# Edit configuration
vim config/sys.config

# Restart server to apply changes
./stop-server.sh
./start-local.sh
```

### Upgrade Erlang/OTP

```bash
# Stop server
./stop-server.sh

# Upgrade Erlang (example using homebrew on macOS)
brew upgrade erlang

# Rebuild
rebar3 clean
rebar3 compile

# Start server
./start-local.sh
```

## Production Deployment

### Systemd Service (Linux)

Create `/etc/systemd/system/erlvectordb.service`:

```ini
[Unit]
Description=ErlVectorDB Vector Database
After=network.target

[Service]
Type=simple
User=erlvectordb
WorkingDirectory=/opt/erlvectordb
ExecStart=/usr/local/bin/rebar3 shell --eval "application:ensure_all_started(erlvectordb)"
ExecStop=/opt/erlvectordb/stop-server.sh
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

**Manage service:**

```bash
# Start
sudo systemctl start erlvectordb

# Stop
sudo systemctl stop erlvectordb

# Enable on boot
sudo systemctl enable erlvectordb

# Check status
sudo systemctl status erlvectordb
```

### Docker Deployment

```bash
# Build image
docker build -t erlvectordb .

# Run container
docker run -d \
  -p 8080:8080 \
  -p 8081:8081 \
  -p 8082:8082 \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/backups:/app/backups \
  --name erlvectordb \
  erlvectordb

# Stop container
docker stop erlvectordb

# View logs
docker logs -f erlvectordb
```

### Monitoring

**Health checks:**

```bash
# Add to cron for monitoring
*/5 * * * * /opt/erlvectordb/check-status.sh || /opt/erlvectordb/start-local.sh
```

**Log rotation:**

```bash
# Add to logrotate
cat > /etc/logrotate.d/erlvectordb <<EOF
/opt/erlvectordb/logs/*.log {
    daily
    rotate 7
    compress
    delaycompress
    missingok
    notifempty
}
EOF
```

## Performance Tuning

### Erlang VM Options

Edit `start-local.sh` or create a custom start script:

```bash
# Increase memory
export ERL_MAX_PORTS=4096
export ERL_MAX_ETS_TABLES=1400

# Start with custom options
erl +P 1000000 +Q 65536 -s erlvectordb
```

### Configuration Tuning

```erlang
% In config/sys.config
{erlvectordb, [
    % Increase sync interval for better write performance
    {sync_interval, 60000},  % 60 seconds
    
    % Enable compression to reduce memory
    {compression_enabled, true},
    {compression_algorithm, quantization_8bit},
    
    % Adjust token lifetime
    {token_lifetime, 7200000},  % 2 hours
    
    % Clustering for horizontal scaling
    {cluster_enabled, true},
    {replication_factor, 3}
]}
```

## Security Best Practices

### Change Default Credentials

```erlang
% In config/sys.config
{erlvectordb, [
    {default_client_id, <<"your_client_id">>},
    {default_client_secret, <<"your_secure_secret">>}
]}
```

### Enable HTTPS (Production)

Use a reverse proxy like nginx:

```nginx
server {
    listen 443 ssl;
    server_name vectordb.example.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    location / {
        proxy_pass http://localhost:8082;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Firewall Rules

```bash
# Allow only specific IPs
sudo ufw allow from 192.168.1.0/24 to any port 8080
sudo ufw allow from 192.168.1.0/24 to any port 8081
sudo ufw allow from 192.168.1.0/24 to any port 8082
```

## Getting Help

- **Check Status**: `./check-status.sh`
- **View Logs**: `tail -f logs/startup_*.log`
- **Run Tests**: `./test_server.sh`
- **Documentation**: [README.md](README.md)
- **Quick Start**: [QUICKSTART.md](QUICKSTART.md)

## Summary of Management Scripts

| Script | Purpose | When to Use |
|--------|---------|-------------|
| `start-local.sh` | Start server with setup | First time or after stop |
| `check-status.sh` | Check server health | Verify server is running |
| `stop-server.sh` | Stop and cleanup | Shutdown or restart |
| `test_server.sh` | Run test suite | Verify functionality |

**Typical workflow:**

```bash
# 1. Start
./start-local.sh

# 2. Check
./check-status.sh

# 3. Test
./test_server.sh

# 4. Stop when done
./stop-server.sh
```
