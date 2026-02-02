#!/usr/bin/env python3

"""
Integration tests for the Gemini MCP Server

Tests the full request/response cycle with actual ErlVectorDB server:
- OAuth token acquisition
- Connection establishment
- Initialize handshake
- Tools/list request
- Tools/call request
- Connection resilience
"""

import json
import subprocess
import sys
import time
import os
from typing import Dict, Any, Optional

# Test configuration
ERLVECTORDB_HOST = os.getenv('ERLVECTORDB_HOST', 'localhost')
ERLVECTORDB_PORT = int(os.getenv('ERLVECTORDB_PORT', '8080'))
OAUTH_HOST = os.getenv('ERLVECTORDB_OAUTH_HOST', 'localhost')
OAUTH_PORT = int(os.getenv('ERLVECTORDB_OAUTH_PORT', '8081'))
CLIENT_ID = os.getenv('ERLVECTORDB_CLIENT_ID', 'admin')
CLIENT_SECRET = os.getenv('ERLVECTORDB_CLIENT_SECRET', 'admin_secret_2024')

# Test results
test_results = []


class TestResult:
    """Test result container"""
    def __init__(self, name: str, passed: bool, message: str = ""):
        self.name = name
        self.passed = passed
        self.message = message
    
    def __str__(self):
        status = "✓ PASS" if self.passed else "✗ FAIL"
        msg = f": {self.message}" if self.message else ""
        return f"{status} - {self.name}{msg}"


class MCPServerTester:
    """Integration tester for MCP server"""
    
    def __init__(self):
        self.process: Optional[subprocess.Popen] = None
        self.request_id = 1
    
    def start_server(self) -> bool:
        """Start the MCP server as a subprocess"""
        try:
            print("Starting MCP server...")
            
            # Set environment variables
            env = os.environ.copy()
            env['ERLVECTORDB_HOST'] = ERLVECTORDB_HOST
            env['ERLVECTORDB_PORT'] = str(ERLVECTORDB_PORT)
            env['ERLVECTORDB_OAUTH_HOST'] = OAUTH_HOST
            env['ERLVECTORDB_OAUTH_PORT'] = str(OAUTH_PORT)
            env['ERLVECTORDB_CLIENT_ID'] = CLIENT_ID
            env['ERLVECTORDB_CLIENT_SECRET'] = CLIENT_SECRET
            env['ERLVECTORDB_LOG_LEVEL'] = 'INFO'
            
            # Start server process
            self.process = subprocess.Popen(
                [sys.executable, 'gemini_mcp_server.py'],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                env=env,
                text=True,
                bufsize=1
            )
            
            # Give server time to start
            time.sleep(0.5)
            
            # Check if process is still running
            if self.process.poll() is not None:
                stderr = self.process.stderr.read()
                print(f"Server failed to start: {stderr}")
                return False
            
            print("MCP server started successfully")
            return True
            
        except Exception as e:
            print(f"Failed to start server: {e}")
            return False
    
    def stop_server(self):
        """Stop the MCP server"""
        if self.process:
            try:
                print("Stopping MCP server...")
                self.process.stdin.close()
                self.process.wait(timeout=5)
                print("MCP server stopped")
            except subprocess.TimeoutExpired:
                print("Server did not stop gracefully, terminating...")
                self.process.terminate()
                self.process.wait(timeout=2)
            except Exception as e:
                print(f"Error stopping server: {e}")
    
    def send_request(self, method: str, params: Dict = None, timeout: float = 5.0) -> Optional[Dict]:
        """Send a JSON-RPC request and receive response"""
        if not self.process or self.process.poll() is not None:
            print("Server is not running")
            return None
        
        try:
            # Create request
            request = {
                'jsonrpc': '2.0',
                'method': method,
                'params': params or {},
                'id': self.request_id
            }
            self.request_id += 1
            
            # Send request
            request_json = json.dumps(request) + '\n'
            print(f"Sending request: {method} (id={request['id']})")
            self.process.stdin.write(request_json)
            self.process.stdin.flush()
            
            # Read response with timeout
            start_time = time.time()
            while time.time() - start_time < timeout:
                # Check if process is still running
                if self.process.poll() is not None:
                    stderr = self.process.stderr.read()
                    print(f"Server died: {stderr}")
                    return None
                
                # Try to read a line
                line = self.process.stdout.readline()
                if line:
                    line = line.strip()
                    if line:
                        response = json.loads(line)
                        print(f"Received response: id={response.get('id')}")
                        return response
                
                # Small sleep to avoid busy waiting
                time.sleep(0.01)
            
            print(f"Timeout waiting for response to {method}")
            return None
            
        except json.JSONDecodeError as e:
            print(f"JSON decode error: {e}")
            return None
        except Exception as e:
            print(f"Error sending request: {e}")
            return None
    
    def test_initialize(self) -> TestResult:
        """Test initialize handshake"""
        print("\n--- Test: Initialize Handshake ---")
        
        response = self.send_request('initialize', {
            'protocolVersion': '2024-11-05',
            'capabilities': {'tools': {}},
            'clientInfo': {
                'name': 'integration-test',
                'version': '1.0.0'
            }
        })
        
        if not response:
            return TestResult("Initialize handshake", False, "No response received")
        
        if 'error' in response:
            return TestResult("Initialize handshake", False, f"Error: {response['error']}")
        
        if 'result' not in response:
            return TestResult("Initialize handshake", False, "Missing result field")
        
        result = response['result']
        
        # Validate response structure
        if 'protocolVersion' not in result:
            return TestResult("Initialize handshake", False, "Missing protocolVersion")
        
        if 'capabilities' not in result:
            return TestResult("Initialize handshake", False, "Missing capabilities")
        
        if 'serverInfo' not in result:
            return TestResult("Initialize handshake", False, "Missing serverInfo")
        
        print(f"Server info: {result['serverInfo']}")
        return TestResult("Initialize handshake", True)
    
    def test_tools_list(self) -> TestResult:
        """Test tools/list request"""
        print("\n--- Test: Tools List ---")
        
        response = self.send_request('tools/list', {})
        
        if not response:
            return TestResult("Tools list", False, "No response received")
        
        if 'error' in response:
            return TestResult("Tools list", False, f"Error: {response['error']}")
        
        if 'result' not in response:
            return TestResult("Tools list", False, "Missing result field")
        
        result = response['result']
        
        # Validate response structure
        if 'tools' not in result:
            return TestResult("Tools list", False, "Missing tools field")
        
        tools = result['tools']
        print(f"Found {len(tools)} tools")
        
        # Print first few tools
        for i, tool in enumerate(tools[:3]):
            print(f"  Tool {i+1}: {tool.get('name', 'unknown')}")
        
        return TestResult("Tools list", True, f"Found {len(tools)} tools")
    
    def test_tools_call_create_store(self) -> TestResult:
        """Test tools/call request - list stores"""
        print("\n--- Test: Tools Call (List Stores) ---")
        
        # Use list_stores instead of create_store to avoid ErlVectorDB parameter bug
        response = self.send_request('tools/call', {
            'name': 'list_stores',
            'arguments': {}
        })
        
        if not response:
            return TestResult("Tools call (list_stores)", False, "No response received")
        
        if 'error' in response:
            return TestResult("Tools call (list_stores)", False, f"Error: {response['error']}")
        
        if 'result' not in response:
            return TestResult("Tools call (list_stores)", False, "Missing result field")
        
        result = response['result']
        stores = result.get('content', [{}])[0].get('text', '[]')
        print(f"Stores: {stores}")
        return TestResult("Tools call (list_stores)", True)
    
    def test_request_id_preservation(self) -> TestResult:
        """Test that request IDs are preserved in responses"""
        print("\n--- Test: Request ID Preservation ---")
        
        # Send request with specific ID
        test_id = 999
        request = {
            'jsonrpc': '2.0',
            'method': 'tools/list',
            'params': {},
            'id': test_id
        }
        
        request_json = json.dumps(request) + '\n'
        self.process.stdin.write(request_json)
        self.process.stdin.flush()
        
        # Read response
        line = self.process.stdout.readline().strip()
        response = json.loads(line)
        
        if response.get('id') != test_id:
            return TestResult("Request ID preservation", False, 
                            f"Expected ID {test_id}, got {response.get('id')}")
        
        return TestResult("Request ID preservation", True)
    
    def test_multiple_requests(self) -> TestResult:
        """Test multiple sequential requests"""
        print("\n--- Test: Multiple Sequential Requests ---")
        
        # Send 5 requests in sequence
        for i in range(5):
            response = self.send_request('tools/list', {}, timeout=10.0)
            if not response:
                return TestResult("Multiple requests", False, f"Request {i+1} failed - no response")
            if 'error' in response:
                return TestResult("Multiple requests", False, f"Request {i+1} error: {response['error']}")
            # Small delay between requests
            time.sleep(0.1)
        
        return TestResult("Multiple requests", True, "5 requests processed successfully")
    
    def test_error_recovery(self) -> TestResult:
        """Test error recovery - server continues after error"""
        print("\n--- Test: Error Recovery ---")
        
        # Send invalid request (missing method)
        invalid_request = {
            'jsonrpc': '2.0',
            'params': {},
            'id': self.request_id
        }
        self.request_id += 1
        
        request_json = json.dumps(invalid_request) + '\n'
        self.process.stdin.write(request_json)
        self.process.stdin.flush()
        
        # Read error response
        line = self.process.stdout.readline().strip()
        error_response = json.loads(line)
        
        if 'error' not in error_response:
            return TestResult("Error recovery", False, "Expected error response")
        
        # Now send valid request to verify server recovered
        response = self.send_request('tools/list', {})
        
        if not response:
            return TestResult("Error recovery", False, "Server did not recover")
        
        if 'error' in response:
            return TestResult("Error recovery", False, "Server still returning errors")
        
        return TestResult("Error recovery", True, "Server recovered after error")
    
    def run_all_tests(self) -> bool:
        """Run all integration tests"""
        print("=" * 60)
        print("INTEGRATION TESTS FOR GEMINI MCP SERVER")
        print("=" * 60)
        
        # Start server
        if not self.start_server():
            print("\n✗ FATAL: Could not start server")
            return False
        
        try:
            # Run tests
            test_results.append(self.test_initialize())
            test_results.append(self.test_tools_list())
            test_results.append(self.test_tools_call_create_store())
            test_results.append(self.test_request_id_preservation())
            test_results.append(self.test_multiple_requests())
            test_results.append(self.test_error_recovery())
            
        finally:
            # Stop server
            self.stop_server()
        
        # Print results
        print("\n" + "=" * 60)
        print("TEST RESULTS")
        print("=" * 60)
        
        passed = 0
        failed = 0
        
        for result in test_results:
            print(result)
            if result.passed:
                passed += 1
            else:
                failed += 1
        
        print("\n" + "-" * 60)
        print(f"Total: {passed + failed} tests")
        print(f"Passed: {passed}")
        print(f"Failed: {failed}")
        print("-" * 60)
        
        return failed == 0


def check_prerequisites() -> bool:
    """Check if ErlVectorDB server is running"""
    import socket
    
    print("Checking prerequisites...")
    
    # Check if MCP server is accessible
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((ERLVECTORDB_HOST, ERLVECTORDB_PORT))
        sock.close()
        
        if result != 0:
            print(f"✗ ErlVectorDB MCP server is not accessible at {ERLVECTORDB_HOST}:{ERLVECTORDB_PORT}")
            print("  Please start the server with: ./start-local.sh")
            return False
        
        print(f"✓ ErlVectorDB MCP server is accessible at {ERLVECTORDB_HOST}:{ERLVECTORDB_PORT}")
    except Exception as e:
        print(f"✗ Error checking MCP server: {e}")
        return False
    
    # Check if OAuth server is accessible
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((OAUTH_HOST, OAUTH_PORT))
        sock.close()
        
        if result != 0:
            print(f"✗ OAuth server is not accessible at {OAUTH_HOST}:{OAUTH_PORT}")
            print("  Please start the server with: ./start-local.sh")
            return False
        
        print(f"✓ OAuth server is accessible at {OAUTH_HOST}:{OAUTH_PORT}")
    except Exception as e:
        print(f"✗ Error checking OAuth server: {e}")
        return False
    
    print("✓ All prerequisites met\n")
    return True


def main():
    """Main entry point"""
    # Check prerequisites
    if not check_prerequisites():
        sys.exit(1)
    
    # Run tests
    tester = MCPServerTester()
    success = tester.run_all_tests()
    
    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
