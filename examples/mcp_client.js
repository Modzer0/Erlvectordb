#!/usr/bin/env node

/**
 * ErlVectorDB MCP Client Example
 * 
 * This is a complete example of how to connect to and interact with
 * the ErlVectorDB MCP server with OAuth 2.1 authentication.
 * 
 * Usage:
 *   node mcp_client.js
 * 
 * Environment Variables:
 *   ERLVECTORDB_HOST - MCP server host (default: localhost)
 *   ERLVECTORDB_PORT - MCP server port (default: 8080)
 *   ERLVECTORDB_OAUTH_HOST - OAuth server host (default: localhost)
 *   ERLVECTORDB_OAUTH_PORT - OAuth server port (default: 8081)
 *   ERLVECTORDB_CLIENT_ID - OAuth client ID (default: admin)
 *   ERLVECTORDB_CLIENT_SECRET - OAuth client secret (default: admin_secret_2024)
 */

const net = require('net');
const http = require('http');
const { URL } = require('url');

class ErlVectorDBClient {
    constructor(options = {}) {
        this.host = options.host || process.env.ERLVECTORDB_HOST || 'localhost';
        this.port = options.port || parseInt(process.env.ERLVECTORDB_PORT) || 8080;
        this.oauthHost = options.oauthHost || process.env.ERLVECTORDB_OAUTH_HOST || 'localhost';
        this.oauthPort = options.oauthPort || parseInt(process.env.ERLVECTORDB_OAUTH_PORT) || 8081;
        this.clientId = options.clientId || process.env.ERLVECTORDB_CLIENT_ID || 'admin';
        this.clientSecret = options.clientSecret || process.env.ERLVECTORDB_CLIENT_SECRET || 'admin_secret_2024';
        
        this.accessToken = null;
        this.refreshToken = null;
        this.tokenExpiry = null;
        this.socket = null;
        this.requestId = 1;
        this.responseHandlers = {};
        this.connected = false;
        this.initialized = false;
    }

    async getAccessToken() {
        console.log('🔐 Getting OAuth access token...');
        
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
                        if (res.statusCode === 200 && response.access_token) {
                            this.accessToken = response.access_token;
                            this.refreshToken = response.refresh_token;
                            this.tokenExpiry = Date.now() + (response.expires_in * 1000);
                            console.log('✅ OAuth token obtained successfully');
                            resolve(response.access_token);
                        } else {
                            console.error('❌ OAuth error:', response);
                            reject(new Error(`OAuth error: ${response.error || 'Unknown error'}`));
                        }
                    } catch (error) {
                        reject(new Error(`Failed to parse OAuth response: ${error.message}`));
                    }
                });
            });

            req.on('error', (error) => {
                console.error('❌ OAuth request failed:', error.message);
                reject(error);
            });

            req.write(postData);
            req.end();
        });
    }

    async refreshAccessToken() {
        if (!this.refreshToken) {
            return this.getAccessToken();
        }

        console.log('🔄 Refreshing access token...');
        
        const postData = new URLSearchParams({
            grant_type: 'refresh_token',
            refresh_token: this.refreshToken
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
                        if (res.statusCode === 200 && response.access_token) {
                            this.accessToken = response.access_token;
                            this.refreshToken = response.refresh_token || this.refreshToken;
                            this.tokenExpiry = Date.now() + (response.expires_in * 1000);
                            console.log('✅ Token refreshed successfully');
                            resolve(response.access_token);
                        } else {
                            console.log('🔄 Refresh failed, getting new token...');
                            resolve(this.getAccessToken());
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

    async ensureValidToken() {
        if (!this.accessToken || (this.tokenExpiry && Date.now() >= this.tokenExpiry - 60000)) {
            if (this.refreshToken) {
                await this.refreshAccessToken();
            } else {
                await this.getAccessToken();
            }
        }
    }

    async connect() {
        await this.ensureValidToken();

        console.log(`🔌 Connecting to MCP server at ${this.host}:${this.port}...`);

        return new Promise((resolve, reject) => {
            this.socket = net.createConnection(this.port, this.host, () => {
                console.log('✅ Connected to ErlVectorDB MCP server');
                this.connected = true;
                resolve();
            });

            this.socket.on('error', (error) => {
                console.error('❌ Connection error:', error.message);
                this.connected = false;
                reject(error);
            });

            this.socket.on('close', () => {
                console.log('🔌 Connection closed');
                this.connected = false;
                this.initialized = false;
            });

            this.socket.on('data', (data) => {
                try {
                    const response = JSON.parse(data.toString());
                    this.handleResponse(response);
                } catch (error) {
                    console.error('❌ Failed to parse response:', error.message);
                    console.error('Raw data:', data.toString());
                }
            });
        });
    }

    async sendRequest(method, params = {}, id = null) {
        if (!this.connected) {
            throw new Error('Not connected to MCP server');
        }

        await this.ensureValidToken();

        if (id === null) {
            id = this.requestId++;
        }

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
            this.responseHandlers[id] = { resolve, reject, timestamp: Date.now() };

            const requestData = JSON.stringify(request);
            console.log(`📤 Sending: ${method}`, params);
            
            this.socket.write(requestData, (error) => {
                if (error) {
                    delete this.responseHandlers[id];
                    reject(error);
                }
            });

            // Timeout after 30 seconds
            setTimeout(() => {
                if (this.responseHandlers[id]) {
                    delete this.responseHandlers[id];
                    reject(new Error(`Request timeout for ${method}`));
                }
            }, 30000);
        });
    }

    handleResponse(response) {
        console.log(`📥 Received response for ID ${response.id}`);
        
        if (response.id && this.responseHandlers[response.id]) {
            const handler = this.responseHandlers[response.id];
            delete this.responseHandlers[response.id];

            if (response.error) {
                console.error('❌ MCP Error:', response.error);
                handler.reject(new Error(`MCP Error: ${response.error.message}`));
            } else {
                console.log('✅ Success:', response.result);
                handler.resolve(response.result);
            }
        } else {
            console.log('📥 Unhandled response:', response);
        }
    }

    // MCP Protocol Methods
    async initialize() {
        console.log('🚀 Initializing MCP connection...');
        
        const result = await this.sendRequest('initialize', {
            protocolVersion: '2024-11-05',
            capabilities: {
                tools: {}
            },
            clientInfo: {
                name: 'erlvectordb-example-client',
                version: '1.0.0'
            }
        });

        this.initialized = true;
        console.log('✅ MCP connection initialized');
        return result;
    }

    async listTools() {
        console.log('📋 Listing available tools...');
        return this.sendRequest('tools/list');
    }

    async callTool(name, arguments) {
        console.log(`🔧 Calling tool: ${name}`);
        return this.sendRequest('tools/call', {
            name: name,
            arguments: arguments
        });
    }

    // Vector Database Operations
    async createStore(name) {
        console.log(`🏪 Creating store: ${name}`);
        return this.callTool('create_store', { name: name });
    }

    async insertVector(store, id, vector, metadata = {}) {
        console.log(`📝 Inserting vector ${id} into ${store}`);
        return this.callTool('insert_vector', {
            store: store,
            id: id,
            vector: vector,
            metadata: metadata
        });
    }

    async searchVectors(store, vector, k = 10) {
        console.log(`🔍 Searching in ${store} for ${k} similar vectors`);
        return this.callTool('search_vectors', {
            store: store,
            vector: vector,
            k: k
        });
    }

    async syncStore(store) {
        console.log(`💾 Syncing store: ${store}`);
        return this.callTool('sync_store', { store: store });
    }

    async backupStore(store, backupName) {
        console.log(`💼 Creating backup: ${backupName} for store: ${store}`);
        return this.callTool('backup_store', {
            store: store,
            backup_name: backupName
        });
    }

    async restoreStore(backupPath, newStoreName) {
        console.log(`📦 Restoring store: ${newStoreName} from: ${backupPath}`);
        return this.callTool('restore_store', {
            backup_path: backupPath,
            new_store_name: newStoreName
        });
    }

    async listBackups() {
        console.log('📋 Listing backups...');
        return this.callTool('list_backups', {});
    }

    disconnect() {
        console.log('👋 Disconnecting...');
        if (this.socket) {
            this.socket.end();
            this.socket = null;
        }
        this.connected = false;
        this.initialized = false;
    }
}

// Example usage and demonstration
async function runExample() {
    const client = new ErlVectorDBClient();
    
    try {
        // Connect and initialize
        await client.connect();
        await client.initialize();
        
        // List available tools
        const tools = await client.listTools();
        console.log('\n📋 Available tools:');
        tools.tools.forEach(tool => {
            console.log(`  - ${tool.name}: ${tool.description}`);
        });

        // Create a test store
        const storeName = 'example_store';
        console.log(`\n🏪 Creating store: ${storeName}`);
        await client.createStore(storeName);
        
        // Insert some test vectors
        console.log('\n📝 Inserting test vectors...');
        const testVectors = [
            { id: 'doc1', vector: [1.0, 2.0, 3.0], metadata: { title: 'Document 1', category: 'tech' } },
            { id: 'doc2', vector: [2.0, 3.0, 4.0], metadata: { title: 'Document 2', category: 'science' } },
            { id: 'doc3', vector: [1.5, 2.5, 3.5], metadata: { title: 'Document 3', category: 'tech' } }
        ];

        for (const { id, vector, metadata } of testVectors) {
            await client.insertVector(storeName, id, vector, metadata);
        }
        
        // Search for similar vectors
        console.log('\n🔍 Searching for similar vectors...');
        const queryVector = [1.1, 2.1, 3.1];
        const searchResults = await client.searchVectors(storeName, queryVector, 3);
        
        console.log('Search results:');
        JSON.parse(searchResults.content[0].text).forEach((result, index) => {
            console.log(`  ${index + 1}. ID: ${result[0]}, Distance: ${result[2].toFixed(4)}`);
            console.log(`     Metadata: ${JSON.stringify(result[1])}`);
        });

        // Sync the store
        console.log('\n💾 Syncing store to persistent storage...');
        await client.syncStore(storeName);

        // Create a backup
        console.log('\n💼 Creating backup...');
        const backupName = `example_backup_${Date.now()}`;
        await client.backupStore(storeName, backupName);

        // List backups
        console.log('\n📋 Listing backups...');
        const backups = await client.listBackups();
        const backupList = JSON.parse(backups.content[0].text);
        console.log(`Found ${backupList.length} backups:`);
        backupList.forEach((backup, index) => {
            console.log(`  ${index + 1}. Store: ${backup.store_name}, Vectors: ${backup.vector_count}, Path: ${backup.file_path}`);
        });

        console.log('\n✅ Example completed successfully!');
        
    } catch (error) {
        console.error('\n❌ Example failed:', error.message);
        if (error.stack) {
            console.error('Stack trace:', error.stack);
        }
    } finally {
        client.disconnect();
    }
}

// Run the example if this file is executed directly
if (require.main === module) {
    console.log('🚀 ErlVectorDB MCP Client Example');
    console.log('=====================================\n');
    
    runExample().then(() => {
        console.log('\n👋 Example finished');
        process.exit(0);
    }).catch((error) => {
        console.error('\n💥 Fatal error:', error);
        process.exit(1);
    });
}

module.exports = ErlVectorDBClient;