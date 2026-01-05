# ErlVectorDB

A high-performance MCP (Model Context Protocol) vector database implemented in Erlang/OTP, designed to take full advantage of the Actor model and OTP supervision trees for fault-tolerant, concurrent vector operations.

## Features

- **OTP-Native Architecture**: Built on gen_servers and supervision trees for maximum fault tolerance
- **MCP Protocol Support**: Full Model Context Protocol implementation for AI/ML integration
- **Persistent Storage**: DETS-based persistence with ETS for fast in-memory operations
- **Backup & Recovery**: Full backup/restore capabilities with JSON export/import
- **Concurrent Vector Operations**: Leverages Erlang's lightweight processes for parallel operations
- **Multiple Distance Metrics**: Cosine similarity, Euclidean distance, Manhattan distance
- **Dynamic Store Management**: Create and manage multiple vector stores at runtime
- **Hot Code Reloading**: Update vector operations without downtime
- **Distributed Ready**: Built for clustering and distribution across nodes

## Architecture

```
erlvectordb_sup (Main Supervisor)
├── vector_store_sup (Dynamic Supervisor for Vector Stores)
│   ├── vector_store (Gen Server per store)
│   │   └── vector_persistence (DETS + ETS storage)
│   └── vector_store (Gen Server per store)
│       └── vector_persistence (DETS + ETS storage)
├── mcp_server (MCP Protocol Handler)
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

% Create a vector store
erlvectordb:create_store(my_store).

% Insert vectors
Vector1 = [1.0, 2.0, 3.0],
erlvectordb:insert(my_store, <<"doc1">>, Vector1, #{title => "Document 1"}).

Vector2 = [2.0, 3.0, 4.0],
erlvectordb:insert(my_store, <<"doc2">>, Vector2, #{title => "Document 2"}).

% Search for similar vectors
QueryVector = [1.1, 2.1, 3.1],
{ok, Results} = erlvectordb:search(my_store, QueryVector, 5).

% Get store statistics
{ok, Stats} = erlvectordb:get_stats(my_store).

% Sync to persistent storage
ok = erlvectordb:sync(my_store).

% Create backup
{ok, BackupInfo} = erlvectordb:backup_store(my_store, "daily_backup").

% List all backups
{ok, Backups} = erlvectordb:list_backups().

% Restore from backup
{ok, RestoreResult} = erlvectordb:restore_store("/path/to/backup.backup", restored_store).
```

## MCP Integration

The database exposes an MCP server on port 8080 (configurable) with the following tools:

- `create_store`: Create a new vector store
- `insert_vector`: Insert a vector with metadata
- `search_vectors`: Search for similar vectors
- `sync_store`: Sync a store to persistent storage
- `backup_store`: Create a backup of a store
- `restore_store`: Restore a store from backup
- `list_backups`: List all available backups

### MCP Client Example

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
  "id": 1
}
```

## Configuration

Edit `config/sys.config`:

```erlang
[
    {erlvectordb, [
        {mcp_port, 8080},
        {max_vector_stores, 100},
        {default_index_type, flat},
        {persistence_enabled, true},
        {persistence_dir, "data"},
        {backup_dir, "backups"},
        {sync_interval, 30000},  % 30 seconds
        {auto_backup_enabled, false},
        {auto_backup_interval, 3600000}  % 1 hour
    ]}
].
```

## Testing

```bash
rebar3 ct
```

## Performance Characteristics

- **Concurrent Inserts**: Each vector store runs in its own process
- **Parallel Search**: Search operations can run concurrently across stores
- **Memory Efficient**: Leverages Erlang's copy-on-write semantics
- **Fault Tolerant**: Individual store crashes don't affect others

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
- [ ] HNSW indexing for approximate nearest neighbor search
- [ ] Distributed clustering support
- [ ] Batch operations
- [ ] Vector quantization
- [ ] Prometheus metrics integration
- [ ] REST API alongside MCP
- [ ] Automatic backup scheduling
- [ ] Compression for stored vectors

## Architecture Benefits

This implementation leverages Erlang/OTP's unique strengths:

1. **Fault Isolation**: Each vector store is isolated in its own process
2. **Hot Code Reloading**: Update algorithms without stopping the database
3. **Massive Concurrency**: Handle thousands of concurrent operations
4. **Distribution**: Easy clustering across multiple nodes
5. **Supervision**: Automatic restart of failed components
6. **Pattern Matching**: Efficient message routing and data processing
7. **Persistent Storage**: DETS provides durability with ETS performance
8. **Backup System**: Built-in backup/restore with JSON interoperability
