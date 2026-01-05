# ErlVectorDB

A high-performance MCP (Model Context Protocol) vector database implemented in Erlang/OTP, designed to take full advantage of the Actor model and OTP supervision trees for fault-tolerant, concurrent vector operations.

## Features

- **OTP-Native Architecture**: Built on gen_servers and supervision trees for maximum fault tolerance
- **MCP Protocol Support**: Full Model Context Protocol implementation for AI/ML integration
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
│   └── vector_store (Gen Server per store)
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
```

## MCP Integration

The database exposes an MCP server on port 8080 (configurable) with the following tools:

- `create_store`: Create a new vector store
- `insert_vector`: Insert a vector with metadata
- `search_vectors`: Search for similar vectors

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
        {persistence_dir, "data"}
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

- [ ] Persistent storage with DETS/Mnesia
- [ ] HNSW indexing for approximate nearest neighbor search
- [ ] Distributed clustering support
- [ ] Batch operations
- [ ] Vector quantization
- [ ] Prometheus metrics integration
- [ ] REST API alongside MCP

## Architecture Benefits

This implementation leverages Erlang/OTP's unique strengths:

1. **Fault Isolation**: Each vector store is isolated in its own process
2. **Hot Code Reloading**: Update algorithms without stopping the database
3. **Massive Concurrency**: Handle thousands of concurrent operations
4. **Distribution**: Easy clustering across multiple nodes
5. **Supervision**: Automatic restart of failed components
6. **Pattern Matching**: Efficient message routing and data processing
