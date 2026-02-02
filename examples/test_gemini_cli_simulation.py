#!/usr/bin/env python
"""
Gemini CLI Simulation Test

Simulates the interaction pattern of Gemini CLI with the MCP server:
1. Start server via stdio
2. Send initialize request
3. Send tools/list request
4. Send multiple tools/call requests
5. Verify no "Connection closed" errors occur
"""

import subprocess
import sys
import json
import time

def test_gemini_cli_pattern():
    """Test the typical Gemini CLI interaction pattern"""
    print("=" * 60)
    print("GEMINI CLI SIMULATION TEST")
    print("=" * 60)
    
    # Start the MCP server
    print("\n1. Starting MCP server...")
    process = subprocess.Popen(
        [sys.executable, 'examples/gemini_mcp_server.py'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    
    time.sleep(0.5)
    
    if process.poll() is not None:
        stderr = process.stderr.read()
        print(f"✗ Server failed to start: {stderr}")
        return False
    
    print("✓ Server started")
    
    try:
        # Test 1: Initialize
        print("\n2. Testing initialize handshake...")
        init_request = {
            'jsonrpc': '2.0',
            'method': 'initialize',
            'params': {
                'protocolVersion': '2024-11-05',
                'capabilities': {'tools': {}},
                'clientInfo': {'name': 'gemini-cli-sim', 'version': '1.0.0'}
            },
            'id': 1
        }
        
        process.stdin.write(json.dumps(init_request) + '\n')
        process.stdin.flush()
        
        response_line = process.stdout.readline().strip()
        response = json.loads(response_line)
        
        if 'error' in response:
            print(f"✗ Initialize failed: {response['error']}")
            return False
        
        print(f"✓ Initialize successful")
        print(f"  Server: {response['result']['serverInfo']['name']}")
        
        # Test 2: Tools list
        print("\n3. Testing tools/list...")
        tools_request = {
            'jsonrpc': '2.0',
            'method': 'tools/list',
            'params': {},
            'id': 2
        }
        
        process.stdin.write(json.dumps(tools_request) + '\n')
        process.stdin.flush()
        
        response_line = process.stdout.readline().strip()
        response = json.loads(response_line)
        
        if 'error' in response:
            print(f"✗ Tools list failed: {response['error']}")
            return False
        
        tools = response['result']['tools']
        print(f"✓ Tools list successful ({len(tools)} tools)")
        
        # Test 3: Multiple tools/call requests
        print("\n4. Testing multiple tools/call requests...")
        store_name = f"test_store_{int(time.time())}"
        
        # Create store
        create_request = {
            'jsonrpc': '2.0',
            'method': 'tools/call',
            'params': {
                'name': 'create_store',
                'arguments': {'name': store_name}
            },
            'id': 3
        }
        
        process.stdin.write(json.dumps(create_request) + '\n')
        process.stdin.flush()
        
        response_line = process.stdout.readline().strip()
        response = json.loads(response_line)
        
        if 'error' in response:
            error_msg = response['error'].get('message', '')
            if 'already exists' not in error_msg.lower():
                print(f"✗ Create store failed: {response['error']}")
                return False
        
        print(f"✓ Create store successful")
        
        # Test 4: Verify no connection closed errors
        print("\n5. Testing connection stability (10 requests)...")
        for i in range(10):
            list_request = {
                'jsonrpc': '2.0',
                'method': 'tools/list',
                'params': {},
                'id': 10 + i
            }
            
            process.stdin.write(json.dumps(list_request) + '\n')
            process.stdin.flush()
            
            response_line = process.stdout.readline().strip()
            response = json.loads(response_line)
            
            if 'error' in response:
                error_msg = response['error'].get('message', '')
                if 'connection closed' in error_msg.lower():
                    print(f"✗ Connection closed error on request {i+1}")
                    return False
                print(f"✗ Error on request {i+1}: {response['error']}")
                return False
            
            if (i + 1) % 5 == 0:
                print(f"  ✓ {i+1}/10 requests completed")
        
        print("✓ All 10 requests completed without connection errors")
        
        # Success!
        print("\n" + "=" * 60)
        print("✓ ALL TESTS PASSED")
        print("=" * 60)
        print("\nNo 'Connection closed' errors detected!")
        print("The MCP server is working correctly with Gemini CLI pattern.")
        
        return True
        
    except Exception as e:
        print(f"\n✗ Test failed with exception: {e}")
        import traceback
        traceback.print_exc()
        return False
        
    finally:
        # Clean shutdown
        print("\n6. Shutting down server...")
        process.stdin.close()
        process.wait(timeout=5)
        print("✓ Server stopped")


if __name__ == '__main__':
    success = test_gemini_cli_pattern()
    sys.exit(0 if success else 1)
