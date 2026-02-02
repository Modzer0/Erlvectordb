#!/usr/bin/env python3

"""
Remaining property-based tests for Gemini MCP Server

Feature: gemini-mcp-server-fix
Tests for OAuth token inclusion and UTF-8 encoding properties.
"""

import json
import socket
import threading
import time
from typing import Dict, Any
from unittest.mock import Mock, patch, MagicMock
from hypothesis import given, strategies as st, settings, assume
import pytest

# Import the classes we're testing
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))

from gemini_mcp_server import (
    SocketHandler,
    OAuthManager,
    RequestRouter,
    ServerConfig,
    AuthenticationError
)


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 6: OAuth token inclusion
@given(
    method=st.sampled_from(['tools/list', 'tools/call']),
    params=st.dictionaries(
        keys=st.text(min_size=1, max_size=20),
        values=st.one_of(st.text(max_size=100), st.integers(), st.booleans()),
        min_size=0,
        max_size=5
    ),
    request_id=st.one_of(st.integers(), st.text(min_size=1, max_size=50))
)
@settings(max_examples=100, deadline=5000)
def test_oauth_token_inclusion_property(method: str, params: Dict[str, Any], request_id):
    """
    Property 6: OAuth token inclusion
    
    For any request forwarded to ErlVectorDB, the request should include 
    a valid OAuth bearer token in the auth field.
    
    Validates: Requirements 2.3, 8.4
    """
    # Create config and components
    config = ServerConfig()
    oauth_manager = OAuthManager(config)
    
    # Mock the OAuth token request to return a test token
    test_token = 'test_oauth_token_12345'
    
    def mock_request_token():
        return {
            'access_token': test_token,
            'token_type': 'Bearer',
            'expires_in': 3600
        }
    
    with patch.object(oauth_manager, '_request_token', side_effect=mock_request_token):
        # Get token to cache it
        token = oauth_manager.get_token()
        assert token == test_token
    
    # Create a mock server to capture the forwarded request
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('localhost', 0))
    server_socket.listen(1)
    port = server_socket.getsockname()[1]
    
    captured_request = [None]
    server_ready = threading.Event()
    
    def mock_server():
        """Mock server that captures the request"""
        try:
            server_ready.set()
            client_socket, _ = server_socket.accept()
            
            # Read the request
            buffer = b""
            while True:
                chunk = client_socket.recv(4096)
                if not chunk:
                    break
                buffer += chunk
                try:
                    # Try to parse as JSON
                    request_data = json.loads(buffer.decode('utf-8'))
                    captured_request[0] = request_data
                    
                    # Send a mock response
                    response = {
                        'jsonrpc': '2.0',
                        'result': {'status': 'ok'},
                        'id': request_data.get('id')
                    }
                    client_socket.sendall(json.dumps(response).encode('utf-8'))
                    break
                except (json.JSONDecodeError, UnicodeDecodeError):
                    # Need more data
                    continue
            
            client_socket.close()
        except Exception as e:
            print(f"Mock server error: {e}")
        finally:
            server_socket.close()
    
    server_thread = threading.Thread(target=mock_server, daemon=True)
    server_thread.start()
    
    # Wait for server to be ready
    server_ready.wait(timeout=2)
    time.sleep(0.1)
    
    try:
        # Create socket handler and connect
        socket_handler = SocketHandler('localhost', port, timeout=5)
        socket_handler.connect()
        
        # Create request router
        request_router = RequestRouter(socket_handler, oauth_manager)
        
        # Create a request to forward
        client_request = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params,
            'id': request_id
        }
        
        # Handle the request (will forward to mock server)
        response = request_router.handle_request(client_request)
        
        # Wait for server to capture the request
        time.sleep(0.2)
        
        # Verify the forwarded request includes OAuth token
        assert captured_request[0] is not None, "Server did not capture request"
        
        forwarded_request = captured_request[0]
        
        # Check that auth field exists
        assert 'auth' in forwarded_request, \
            "Forwarded request missing 'auth' field"
        
        # Check auth structure
        auth = forwarded_request['auth']
        assert isinstance(auth, dict), "Auth field must be a dictionary"
        assert 'type' in auth, "Auth missing 'type' field"
        assert 'token' in auth, "Auth missing 'token' field"
        
        # Check auth values
        assert auth['type'] == 'bearer', \
            f"Auth type should be 'bearer', got '{auth['type']}'"
        assert auth['token'] == test_token, \
            f"Auth token should be '{test_token}', got '{auth['token']}'"
        
        socket_handler.close()
        
    except Exception as e:
        pytest.fail(f"Property test failed: {e}")
    finally:
        server_thread.join(timeout=2)


# Feature: gemini-mcp-server-fix, Property 11: UTF-8 encoding
@given(
    message=st.dictionaries(
        keys=st.text(min_size=1, max_size=50),
        values=st.one_of(
            st.text(min_size=0, max_size=500),  # Include Unicode characters
            st.integers(),
            st.floats(allow_nan=False, allow_infinity=False),
            st.booleans(),
            st.none()
        ),
        min_size=1,
        max_size=10
    )
)
@settings(max_examples=100, deadline=5000)
def test_utf8_encoding_property(message: Dict[str, Any]):
    """
    Property 11: UTF-8 encoding
    
    For any request sent to ErlVectorDB, the JSON-RPC message should be 
    encoded as UTF-8.
    
    Validates: Requirements 5.1
    """
    # Create a mock server that captures the raw bytes
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('localhost', 0))
    server_socket.listen(1)
    port = server_socket.getsockname()[1]
    
    captured_bytes = [b""]
    server_ready = threading.Event()
    
    def mock_server():
        """Mock server that captures raw bytes"""
        try:
            server_ready.set()
            client_socket, _ = server_socket.accept()
            
            # Read all data
            buffer = b""
            while True:
                chunk = client_socket.recv(4096)
                if not chunk:
                    break
                buffer += chunk
                
                # Try to decode as UTF-8 and parse as JSON
                try:
                    decoded = buffer.decode('utf-8')
                    parsed = json.loads(decoded)
                    
                    # Store the raw bytes
                    captured_bytes[0] = buffer
                    
                    # Send a response
                    response = json.dumps({
                        'jsonrpc': '2.0',
                        'result': {'received': True},
                        'id': parsed.get('id')
                    }).encode('utf-8')
                    client_socket.sendall(response)
                    break
                except (UnicodeDecodeError, json.JSONDecodeError):
                    # Need more data or not UTF-8
                    continue
            
            client_socket.close()
        except Exception as e:
            print(f"Mock server error: {e}")
        finally:
            server_socket.close()
    
    server_thread = threading.Thread(target=mock_server, daemon=True)
    server_thread.start()
    
    # Wait for server to be ready
    server_ready.wait(timeout=2)
    time.sleep(0.1)
    
    try:
        # Create socket handler and connect
        handler = SocketHandler('localhost', port, timeout=5)
        handler.connect()
        
        # Send the message
        handler.send_message(message)
        
        # Receive response to ensure send completed
        response = handler.receive_message()
        
        # Wait for server to capture bytes
        time.sleep(0.2)
        
        # Verify the captured bytes are valid UTF-8
        assert len(captured_bytes[0]) > 0, "Server did not capture any bytes"
        
        # Test 1: Bytes should be decodable as UTF-8
        try:
            decoded_str = captured_bytes[0].decode('utf-8')
        except UnicodeDecodeError as e:
            pytest.fail(f"Captured bytes are not valid UTF-8: {e}")
        
        # Test 2: Decoded string should be valid JSON
        try:
            decoded_message = json.loads(decoded_str)
        except json.JSONDecodeError as e:
            pytest.fail(f"Decoded UTF-8 string is not valid JSON: {e}")
        
        # Test 3: Decoded message should match original message
        assert decoded_message == message, \
            f"Decoded message does not match original"
        
        # Test 4: Verify UTF-8 encoding explicitly
        # Re-encode the message and compare
        expected_bytes = json.dumps(message).encode('utf-8')
        assert captured_bytes[0] == expected_bytes, \
            "Message was not encoded as UTF-8"
        
        handler.close()
        
    except Exception as e:
        pytest.fail(f"Property test failed: {e}")
    finally:
        server_thread.join(timeout=2)


# ============================================================================
# Unit Tests for Additional Coverage
# ============================================================================

def test_oauth_token_in_initialize():
    """Test that initialize request includes OAuth token"""
    config = ServerConfig()
    oauth_manager = OAuthManager(config)
    
    # Mock token request
    test_token = 'init_test_token'
    
    def mock_request_token():
        return {
            'access_token': test_token,
            'token_type': 'Bearer',
            'expires_in': 3600
        }
    
    with patch.object(oauth_manager, '_request_token', side_effect=mock_request_token):
        # Create mock server
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(1)
        port = server_socket.getsockname()[1]
        
        captured_request = [None]
        
        def mock_server():
            try:
                client_socket, _ = server_socket.accept()
                
                # Read request
                buffer = b""
                while True:
                    chunk = client_socket.recv(4096)
                    if not chunk:
                        break
                    buffer += chunk
                    try:
                        request_data = json.loads(buffer.decode('utf-8'))
                        captured_request[0] = request_data
                        
                        # Send response
                        response = {
                            'jsonrpc': '2.0',
                            'result': {
                                'protocolVersion': '2024-11-05',
                                'capabilities': {'tools': {}},
                                'serverInfo': {'name': 'test'}
                            },
                            'id': request_data.get('id')
                        }
                        client_socket.sendall(json.dumps(response).encode('utf-8'))
                        break
                    except (json.JSONDecodeError, UnicodeDecodeError):
                        continue
                
                client_socket.close()
            finally:
                server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        time.sleep(0.1)
        
        try:
            # Create components
            socket_handler = SocketHandler('localhost', port, timeout=5)
            request_router = RequestRouter(socket_handler, oauth_manager)
            
            # Handle initialize request
            response = request_router.handle_initialize({}, 1)
            
            time.sleep(0.2)
            
            # Verify token was included
            assert captured_request[0] is not None
            assert 'auth' in captured_request[0]
            assert captured_request[0]['auth']['token'] == test_token
            
            socket_handler.close()
            
        finally:
            server_thread.join(timeout=2)


def test_utf8_with_special_characters():
    """Test UTF-8 encoding with special Unicode characters"""
    # Create message with various Unicode characters
    message = {
        'emoji': '🚀🎉',
        'chinese': '你好世界',
        'arabic': 'مرحبا',
        'russian': 'Привет',
        'math': '∑∫∂',
        'mixed': 'Hello 世界 🌍'
    }
    
    # Create mock server
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('localhost', 0))
    server_socket.listen(1)
    port = server_socket.getsockname()[1]
    
    captured_bytes = [b""]
    
    def mock_server():
        try:
            client_socket, _ = server_socket.accept()
            
            buffer = b""
            while True:
                chunk = client_socket.recv(4096)
                if not chunk:
                    break
                buffer += chunk
                try:
                    decoded = buffer.decode('utf-8')
                    parsed = json.loads(decoded)
                    captured_bytes[0] = buffer
                    
                    response = json.dumps({
                        'jsonrpc': '2.0',
                        'result': {'ok': True},
                        'id': 1
                    }).encode('utf-8')
                    client_socket.sendall(response)
                    break
                except (UnicodeDecodeError, json.JSONDecodeError):
                    continue
            
            client_socket.close()
        finally:
            server_socket.close()
    
    server_thread = threading.Thread(target=mock_server, daemon=True)
    server_thread.start()
    time.sleep(0.1)
    
    try:
        handler = SocketHandler('localhost', port, timeout=5)
        handler.connect()
        
        handler.send_message(message)
        response = handler.receive_message()
        
        time.sleep(0.2)
        
        # Verify UTF-8 encoding
        decoded = captured_bytes[0].decode('utf-8')
        decoded_message = json.loads(decoded)
        
        assert decoded_message == message
        assert decoded_message['emoji'] == '🚀🎉'
        assert decoded_message['chinese'] == '你好世界'
        
        handler.close()
        
    finally:
        server_thread.join(timeout=2)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
