#!/usr/bin/env python3

"""
ErlVectorDB MCP Client Example

This is a complete example of how to connect to and interact with
the ErlVectorDB MCP server with OAuth 2.1 authentication.

Usage:
    python mcp_client.py

Environment Variables:
    ERLVECTORDB_HOST - MCP server host (default: localhost)
    ERLVECTORDB_PORT - MCP server port (default: 8080)
    ERLVECTORDB_OAUTH_HOST - OAuth server host (default: localhost)
    ERLVECTORDB_OAUTH_PORT - OAuth server port (default: 8081)
    ERLVECTORDB_CLIENT_ID - OAuth client ID (default: admin)
    ERLVECTORDB_CLIENT_SECRET - OAuth client secret (default: admin_secret_2024)
"""

import json
import socket
import requests
import time
import os
from typing import Dict, List, Any, Optional
from urllib.parse import urlencode

class ErlVectorDBClient:
    def __init__(self, host=None, port=None, oauth_host=None, oauth_port=None, 
                 client_id=None, client_secret=None):
        self.host = host or os.getenv('ERLVECTORDB_HOST', 'localhost')
        self.port = int(port or os.getenv('ERLVECTORDB_PORT', '8080'))
        self.oauth_host = oauth_host or os.getenv('ERLVECTORDB_OAUTH_HOST', 'localhost')
        self.oauth_port = int(oauth_port or os.getenv('ERLVECTORDB_OAUTH_PORT', '8081'))
        self.client_id = client_id or os.getenv('ERLVECTORDB_CLIENT_ID', 'admin')
        self.client_secret = client_secret or os.getenv('ERLVECTORDB_CLIENT_SECRET', 'admin_secret_2024')
        
        self.access_token = None
        self.refresh_token = None
        self.token_expiry = None
        self.socket = None
        self.request_id = 1
        self.connected = False
        self.initialized = False

    def get_access_token(self) -> str:
        """Get OAuth access token"""
        print("🔐 Getting OAuth access token...")
        
        url = f'http://{self.oauth_host}:{self.oauth_port}/oauth/token'
        data = {
            'grant_type': 'client_credentials',
            'client_id': self.client_id,
            'client_secret': self.client_secret,
            'scope': 'read write admin'
        }
        
        try:
            response = requests.post(url, data=data, timeout=10)
            response.raise_for_status()
            
            token_data = response.json()
            self.access_token = token_data['access_token']
            self.refresh_token = token_data.get('refresh_token')
            self.token_expiry = time.time() + token_data.get('expires_in', 3600)
            
            print("✅ OAuth token obtained successfully")
            return self.access_token
            
        except requests.exceptions.RequestException as e:
            print(f"❌ OAuth request failed: {e}")
            raise
        except KeyError as e:
            print(f"❌ OAuth response missing required field: {e}")
            raise

    def refresh_access_token(self) -> str:
        """Refresh OAuth access token"""
        if not self.refresh_token:
            return self.get_access_token()

        print("🔄 Refreshing access token...")
        
        url = f'http://{self.oauth_host}:{self.oauth_port}/oauth/token'
        data = {
            'grant_type': 'refresh_token',
            'refresh_token': self.refresh_token
        }
        
        try:
            response = requests.post(url, data=data, timeout=10)
            
            if response.status_code == 200:
                token_data = response.json()
                self.access_token = token_data['access_token']
                self.refresh_token = token_data.get('refresh_token', self.refresh_token)
                self.token_expiry = time.time() + token_data.get('expires_in', 3600)
                print("✅ Token refreshed successfully")
                return self.access_token
            else:
                print("🔄 Refresh failed, getting new token...")
                return self.get_access_token()
                
        except requests.exceptions.RequestException:
            print("🔄 Refresh failed, getting new token...")
            return self.get_access_token()

    def ensure_valid_token(self):
        """Ensure we have a valid access token"""
        if not self.access_token or (self.token_expiry and time.time() >= self.token_expiry - 60):
            if self.refresh_token:
                self.refresh_access_token()
            else:
                self.get_access_token()

    def connect(self):
        """Connect to MCP server"""
        self.ensure_valid_token()
        
        print(f"🔌 Connecting to MCP server at {self.host}:{self.port}...")
        
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(10)  # 10 second timeout
            self.socket.connect((self.host, self.port))
            self.connected = True
            print("✅ Connected to ErlVectorDB MCP server")
            
        except socket.error as e:
            print(f"❌ Connection failed: {e}")
            self.connected = False
            raise

    def send_request(self, method: str, params: Dict = None, request_id: int = None) -> Dict:
        """Send MCP request"""
        if not self.connected:
            raise Exception("Not connected to MCP server")

        self.ensure_valid_token()

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

        print(f"📤 Sending: {method}", params or {})
        
        try:
            request_data = json.dumps(request).encode('utf-8')
            self.socket.send(request_data)

            # Receive response with timeout
            self.socket.settimeout(30)  # 30 second timeout for response
            response_data = self.socket.recv(8192)  # Increased buffer size
            
            if not response_data:
                raise Exception("No response received from server")
                
            response = json.loads(response_data.decode('utf-8'))
            
            print(f"📥 Received response for ID {response.get('id')}")

            if 'error' in response:
                print(f"❌ MCP Error: {response['error']}")
                raise Exception(f"MCP Error: {response['error']['message']}")

            print("✅ Success:", response.get('result', {}))
            return response.get('result', {})
            
        except socket.timeout:
            raise Exception(f"Request timeout for {method}")
        except json.JSONDecodeError as e:
            print(f"❌ Failed to parse response: {e}")
            print(f"Raw response: {response_data}")
            raise
        except Exception as e:
            print(f"❌ Request failed: {e}")
            raise

    def initialize(self) -> Dict:
        """Initialize MCP connection"""
        print("🚀 Initializing MCP connection...")
        
        result = self.send_request('initialize', {
            'protocolVersion': '2024-11-05',
            'capabilities': {'tools': {}},
            'clientInfo': {
                'name': 'erlvectordb-python-example',
                'version': '1.0.0'
            }
        })
        
        self.initialized = True
        print("✅ MCP connection initialized")
        return result

    def list_tools(self) -> Dict:
        """List available tools"""
        print("📋 Listing available tools...")
        return self.send_request('tools/list')

    def call_tool(self, name: str, arguments: Dict) -> Dict:
        """Call a specific tool"""
        print(f"🔧 Calling tool: {name}")
        return self.send_request('tools/call', {
            'name': name,
            'arguments': arguments
        })

    # Vector Database Operations
    def create_store(self, name: str) -> Dict:
        print(f"🏪 Creating store: {name}")
        return self.call_tool('create_store', {'name': name})

    def insert_vector(self, store: str, vector_id: str, vector: List[float], 
                     metadata: Dict = None) -> Dict:
        print(f"📝 Inserting vector {vector_id} into {store}")
        return self.call_tool('insert_vector', {
            'store': store,
            'id': vector_id,
            'vector': vector,
            'metadata': metadata or {}
        })

    def search_vectors(self, store: str, vector: List[float], k: int = 10) -> Dict:
        print(f"🔍 Searching in {store} for {k} similar vectors")
        return self.call_tool('search_vectors', {
            'store': store,
            'vector': vector,
            'k': k
        })

    def sync_store(self, store: str) -> Dict:
        print(f"💾 Syncing store: {store}")
        return self.call_tool('sync_store', {'store': store})

    def backup_store(self, store: str, backup_name: str) -> Dict:
        print(f"💼 Creating backup: {backup_name} for store: {store}")
        return self.call_tool('backup_store', {
            'store': store,
            'backup_name': backup_name
        })

    def restore_store(self, backup_path: str, new_store_name: str) -> Dict:
        print(f"📦 Restoring store: {new_store_name} from: {backup_path}")
        return self.call_tool('restore_store', {
            'backup_path': backup_path,
            'new_store_name': new_store_name
        })

    def list_backups(self) -> Dict:
        print("📋 Listing backups...")
        return self.call_tool('list_backups', {})

    def disconnect(self):
        """Disconnect from server"""
        print("👋 Disconnecting...")
        if self.socket:
            try:
                self.socket.close()
            except:
                pass
            self.socket = None
        self.connected = False
        self.initialized = False

def run_example():
    """Run the example demonstration"""
    client = ErlVectorDBClient()
    
    try:
        # Connect and initialize
        client.connect()
        client.initialize()
        
        # List available tools
        tools = client.list_tools()
        print('\n📋 Available tools:')
        for tool in tools['tools']:
            print(f"  - {tool['name']}: {tool['description']}")

        # Create a test store
        store_name = 'python_example_store'
        print(f'\n🏪 Creating store: {store_name}')
        client.create_store(store_name)
        
        # Insert some test vectors
        print('\n📝 Inserting test vectors...')
        test_vectors = [
            {'id': 'doc1', 'vector': [1.0, 2.0, 3.0], 'metadata': {'title': 'Document 1', 'category': 'tech'}},
            {'id': 'doc2', 'vector': [2.0, 3.0, 4.0], 'metadata': {'title': 'Document 2', 'category': 'science'}},
            {'id': 'doc3', 'vector': [1.5, 2.5, 3.5], 'metadata': {'title': 'Document 3', 'category': 'tech'}}
        ]

        for vector_data in test_vectors:
            client.insert_vector(
                store_name, 
                vector_data['id'], 
                vector_data['vector'], 
                vector_data['metadata']
            )
        
        # Search for similar vectors
        print('\n🔍 Searching for similar vectors...')
        query_vector = [1.1, 2.1, 3.1]
        search_results = client.search_vectors(store_name, query_vector, 3)
        
        print('Search results:')
        results = json.loads(search_results['content'][0]['text'])
        for i, result in enumerate(results):
            print(f"  {i + 1}. ID: {result[0]}, Distance: {result[2]:.4f}")
            print(f"     Metadata: {result[1]}")

        # Sync the store
        print('\n💾 Syncing store to persistent storage...')
        client.sync_store(store_name)

        # Create a backup
        print('\n💼 Creating backup...')
        backup_name = f'python_example_backup_{int(time.time())}'
        client.backup_store(store_name, backup_name)

        # List backups
        print('\n📋 Listing backups...')
        backups = client.list_backups()
        backup_list = json.loads(backups['content'][0]['text'])
        print(f'Found {len(backup_list)} backups:')
        for i, backup in enumerate(backup_list):
            print(f"  {i + 1}. Store: {backup['store_name']}, Vectors: {backup['vector_count']}")
            print(f"     Path: {backup['file_path']}")

        print('\n✅ Example completed successfully!')
        
    except Exception as e:
        print(f'\n❌ Example failed: {e}')
        import traceback
        traceback.print_exc()
    finally:
        client.disconnect()

if __name__ == '__main__':
    print('🚀 ErlVectorDB MCP Client Example (Python)')
    print('==========================================\n')
    
    try:
        run_example()
        print('\n👋 Example finished')
    except KeyboardInterrupt:
        print('\n⚠️  Interrupted by user')
    except Exception as e:
        print(f'\n💥 Fatal error: {e}')
        exit(1)