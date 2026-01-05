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

### Prerequisites

- Erlang/OTP 24+
- Rebar3

### Building

```bash
rebar3 compile
```

### Running

```bash
rebar3 shell
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

## MCP Integration

The database exposes an MCP server on port 8080 (configurable) with OAuth 2.1 authentication. OAuth server runs on port 8081.

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
