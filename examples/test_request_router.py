#!/usr/bin/env python3

"""
Tests for RequestRouter class

This module contains both property-based tests and unit tests for the RequestRouter
class that routes MCP requests to appropriate handlers with error handling.
"""

import json
from typing import Dict, Any
from unittest.mock import Mock, patch, MagicMock
from hypothesis import given, strategies as st, settings, assume
import pytest

# Import the RequestRouter class
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))
from gemini_mcp_server import (
    RequestRouter, SocketHandler, OAuthManager, ServerConfig,
    InvalidRequestError, MethodNotFoundError, AuthenticationError
)


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 2: Request ID preservation
@given(
    method=st.sampled_from(['initialize', 'tools/list', 'tools/call']),
    request_id=st.one_of(
        st.integers(),
        st.text(min_size=1, max_size=50),
        st.none()
    )
)
@settings(max_examples=100, deadline=3000)
def test_request_id_preservation_property(method, request_id):
    """
    Property 2: Request ID preservation
    
    For any JSON-RPC request with an ID, the response should contain 
    the same ID value.
    
    Validates: Requirements 2.5, 2.2, 5.4
    """
    # Create mock dependencies
    mock_socket = Mock(spec=SocketHandler)
    mock_oauth = Mock(spec=OAuthManager)
    
    # Configure mocks
    mock_socket.is_connected.return_value = True
    mock_oauth.get_auth_dict.return_value = {'type': 'bearer', 'token': 'test_token'}
    
    # Mock socket responses based on method
    if method == 'initialize':
        mock_response = {
            'jsonrpc': '2.0',
            'result': {
                'protocolVersion': '2024-11-05',
                'capabilities': {'tools': {}},
                'serverInfo': {'name': 'test', 'version': '1.0.0'}
            },
            'id': 999
        }
    else:
        mock_response = {
            'jsonrpc': '2.0',
            'result': {'status': 'ok'},
            'id': 999
        }
    
    mock_socket.receive_message.return_value = mock_response
    
    # Create router
    router = RequestRouter(mock_socket, mock_oauth)
    
    # Create request with the given ID
    request = {
        'jsonrpc': '2.0',
        'method': method,
        'params': {} if method != 'tools/call' else {'name': 'test_tool'},
        'id': request_id
    }
    
    # Handle request
    response = router.handle_request(request)
    
    # Verify response has same ID as request
    assert response['id'] == request_id, \
        f"Response ID {response['id']} does not match request ID {request_id}"
    
    # Verify response is valid JSON-RPC
    assert response['jsonrpc'] == '2.0'
    assert 'result' in response or 'error' in response


# Feature: gemini-mcp-server-fix, Property 3: Error recovery and continuation
@given(
    error_type=st.sampled_from(['parse', 'invalid_request', 'method_not_found', 'connection', 'internal']),
    request_id=st.one_of(st.integers(), st.text(min_size=1, max_size=20), st.none())
)
@settings(max_examples=100, deadline=3000)
def test_error_recovery_and_continuation_property(error_type, request_id):
    """
    Property 3: Error recovery and continuation
    
    For any non-fatal error (parse error, invalid request, processing error), 
    the server should return an error response and continue processing 
    subsequent requests.
    
    Validates: Requirements 1.5, 6.5, 4.5
    """
    # Create mock dependencies
    mock_socket = Mock(spec=SocketHandler)
    mock_oauth = Mock(spec=OAuthManager)
    
    # Configure mocks
    mock_socket.is_connected.return_value = True
    mock_oauth.get_auth_dict.return_value = {'type': 'bearer', 'token': 'test_token'}
    
    # Create router
    router = RequestRouter(mock_socket, mock_oauth)
    
    # Create request that will trigger the specified error
    if error_type == 'parse':
        # Invalid JSON structure (missing jsonrpc)
        request = {'method': 'test', 'id': request_id}
    elif error_type == 'invalid_request':
        # Invalid request (missing method)
        request = {'jsonrpc': '2.0', 'id': request_id}
    elif error_type == 'method_not_found':
        # Unknown method
        request = {'jsonrpc': '2.0', 'method': 'unknown_method', 'id': request_id}
    elif error_type == 'connection':
        # Connection error
        request = {'jsonrpc': '2.0', 'method': 'tools/list', 'id': request_id}
        mock_socket.is_connected.return_value = False
        mock_socket.send_message.side_effect = ConnectionError("Connection failed")
    else:  # internal error
        # Internal error (exception in handler)
        request = {'jsonrpc': '2.0', 'method': 'initialize', 'id': request_id}
        mock_socket.is_connected.return_value = False  # Force connect() to be called
        mock_socket.connect.side_effect = Exception("Internal error")
    
    # Handle request - should return error response, not raise exception
    response = router.handle_request(request)
    
    # Verify error response structure
    assert response['jsonrpc'] == '2.0'
    assert 'error' in response
    assert 'code' in response['error']
    assert 'message' in response['error']
    assert response['id'] == request_id
    
    # Verify appropriate error code
    if error_type == 'parse' or error_type == 'invalid_request':
        assert response['error']['code'] in [-32700, -32600]
    elif error_type == 'method_not_found':
        assert response['error']['code'] == -32601
    elif error_type == 'connection':
        assert response['error']['code'] == -32000
    else:  # internal
        assert response['error']['code'] == -32603
    
    # Verify router is still functional (can process next request)
    # Create a valid request
    valid_request = {
        'jsonrpc': '2.0',
        'method': 'initialize',
        'params': {},
        'id': 'next_request'
    }
    
    # Reset mocks for next request
    mock_socket.is_connected.return_value = True
    mock_socket.connect.side_effect = None
    mock_socket.send_message.side_effect = None
    mock_socket.receive_message.return_value = {
        'jsonrpc': '2.0',
        'result': {'protocolVersion': '2024-11-05', 'capabilities': {}, 'serverInfo': {}},
        'id': 999
    }
    
    # Should be able to process next request
    next_response = router.handle_request(valid_request)
    assert next_response['jsonrpc'] == '2.0'
    assert next_response['id'] == 'next_request'


# ============================================================================
# Unit Tests
# ============================================================================

class TestRequestRouterInitialize:
    """Unit tests for initialize request handling"""
    
    def test_initialize_request_handling(self):
        """Test initialize request handling"""
        # Create mock dependencies
        mock_socket = Mock(spec=SocketHandler)
        mock_oauth = Mock(spec=OAuthManager)
        
        # Configure mocks
        mock_socket.is_connected.return_value = False
        mock_oauth.get_auth_dict.return_value = {'type': 'bearer', 'token': 'test_token'}
        
        # Mock initialize response from ErlVectorDB
        mock_socket.receive_message.return_value = {
            'jsonrpc': '2.0',
            'result': {
                'protocolVersion': '2024-11-05',
                'capabilities': {'tools': {}},
                'serverInfo': {'name': 'erlvectordb', 'version': '0.1.0'}
            },
            'id': 1
        }
        
        # Create router
        router = RequestRouter(mock_socket, mock_oauth)
        
        # Create initialize request
        request = {
            'jsonrpc': '2.0',
            'method': 'initialize',
            'params': {
                'protocolVersion': '2024-11-05',
                'capabilities': {'tools': {}}
            },
            'id': 1
        }
        
        # Handle request
        response = router.handle_request(request)
        
        # Verify response
        assert response['jsonrpc'] == '2.0'
        assert response['id'] == 1
        assert 'result' in response
        assert response['result']['protocolVersion'] == '2024-11-05'
        assert 'serverInfo' in response['result']
        
        # Verify socket was connected
        mock_socket.connect.assert_called_once()


class TestRequestRouterToolsList:
    """Unit tests for tools/list request handling"""
    
    def test_tools_list_request_handling(self):
        """Test tools/list request handling"""
        # Create mock dependencies
        mock_socket = Mock(spec=SocketHandler)
        mock_oauth = Mock(spec=OAuthManager)
        
        # Configure mocks
        mock_socket.is_connected.return_value = True
        mock_oauth.get_auth_dict.return_value = {'type': 'bearer', 'token': 'test_token'}
        
        # Mock tools/list response from ErlVectorDB
        mock_socket.receive_message.return_value = {
            'jsonrpc': '2.0',
            'result': {
                'tools': [
                    {'name': 'insert_vector', 'description': 'Insert a vector'},
                    {'name': 'search_vectors', 'description': 'Search vectors'}
                ]
            },
            'id': 2
        }
        
        # Create router
        router = RequestRouter(mock_socket, mock_oauth)
        
        # Create tools/list request
        request = {
            'jsonrpc': '2.0',
            'method': 'tools/list',
            'params': {},
            'id': 2
        }
        
        # Handle request
        response = router.handle_request(request)
        
        # Verify response
        assert response['jsonrpc'] == '2.0'
        assert response['id'] == 2
        assert 'result' in response
        assert 'tools' in response['result']
        
        # Verify request was forwarded
        mock_socket.send_message.assert_called_once()


class TestRequestRouterToolsCall:
    """Unit tests for tools/call request handling"""
    
    def test_tools_call_request_handling(self):
        """Test tools/call request handling"""
        # Create mock dependencies
        mock_socket = Mock(spec=SocketHandler)
        mock_oauth = Mock(spec=OAuthManager)
        
        # Configure mocks
        mock_socket.is_connected.return_value = True
        mock_oauth.get_auth_dict.return_value = {'type': 'bearer', 'token': 'test_token'}
        
        # Mock tools/call response from ErlVectorDB
        mock_socket.receive_message.return_value = {
            'jsonrpc': '2.0',
            'result': {
                'content': [{'type': 'text', 'text': 'Vector inserted successfully'}]
            },
            'id': 3
        }
        
        # Create router
        router = RequestRouter(mock_socket, mock_oauth)
        
        # Create tools/call request
        request = {
            'jsonrpc': '2.0',
            'method': 'tools/call',
            'params': {
                'name': 'insert_vector',
                'arguments': {
                    'store': 'test_store',
                    'id': 'doc1',
                    'vector': [1.0, 2.0, 3.0]
                }
            },
            'id': 3
        }
        
        # Handle request
        response = router.handle_request(request)
        
        # Verify response
        assert response['jsonrpc'] == '2.0'
        assert response['id'] == 3
        assert 'result' in response
        
        # Verify request was forwarded
        mock_socket.send_message.assert_called_once()


class TestRequestRouterErrorHandling:
    """Unit tests for error handling"""
    
    def test_invalid_method_handling(self):
        """Test invalid method handling"""
        # Create mock dependencies
        mock_socket = Mock(spec=SocketHandler)
        mock_oauth = Mock(spec=OAuthManager)
        
        # Create router
        router = RequestRouter(mock_socket, mock_oauth)
        
        # Create request with invalid method
        request = {
            'jsonrpc': '2.0',
            'method': 'invalid_method',
            'params': {},
            'id': 1
        }
        
        # Handle request
        response = router.handle_request(request)
        
        # Verify error response
        assert response['jsonrpc'] == '2.0'
        assert response['id'] == 1
        assert 'error' in response
        assert response['error']['code'] == -32601
        assert 'not found' in response['error']['message'].lower()
    
    def test_parse_error_handling(self):
        """Test parse error handling"""
        # Create mock dependencies
        mock_socket = Mock(spec=SocketHandler)
        mock_oauth = Mock(spec=OAuthManager)
        
        # Create router
        router = RequestRouter(mock_socket, mock_oauth)
        
        # Create request with missing jsonrpc field
        request = {
            'method': 'initialize',
            'id': 1
        }
        
        # Handle request
        response = router.handle_request(request)
        
        # Verify error response
        assert response['jsonrpc'] == '2.0'
        assert response['id'] == 1
        assert 'error' in response
        assert response['error']['code'] == -32600


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
