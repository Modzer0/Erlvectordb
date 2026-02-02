#!/usr/bin/env python3

"""
Property-based and unit tests for MCPServer class

Feature: gemini-mcp-server-fix
Tests the main server orchestrator that coordinates all components.
"""

import json
import io
import sys
import signal
import time
from unittest.mock import Mock, MagicMock, patch
from hypothesis import given, strategies as st, settings
import pytest

# Import the classes we're testing
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))

from gemini_mcp_server import (
    MCPServer,
    ServerConfig,
    StdioHandler,
    SocketHandler,
    OAuthManager,
    RequestRouter
)


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 1: Continuous stdin reading
@given(
    requests=st.lists(
        st.fixed_dictionaries({
            'jsonrpc': st.just('2.0'),
            'method': st.sampled_from(['initialize', 'tools/list', 'tools/call']),
            'params': st.dictionaries(st.text(), st.text()),
            'id': st.one_of(st.integers(), st.text())
        }),
        min_size=1,
        max_size=10
    )
)
@settings(max_examples=100, deadline=None)
def test_continuous_stdin_reading_property(requests):
    """
    Property 1: Continuous stdin reading
    
    For any sequence of JSON-RPC requests sent to stdin, the server should
    process each request and return to waiting for the next request until
    EOF is received.
    
    Validates: Requirements 1.1, 6.1
    """
    # Create mock configuration
    config = ServerConfig()
    
    # Create input stream with requests followed by EOF
    input_lines = [json.dumps(req) + '\n' for req in requests]
    input_stream = io.StringIO(''.join(input_lines))
    
    # Create output stream to capture responses
    output_stream = io.StringIO()
    
    # Create server with mocked components
    server = MCPServer(config)
    
    # Replace stdio handler with one using our test streams
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Mock the request router to return simple responses
    def mock_handle_request(request):
        return {
            'jsonrpc': '2.0',
            'result': {'status': 'ok'},
            'id': request.get('id')
        }
    
    server.request_router.handle_request = mock_handle_request
    
    # Run the server (will exit on EOF)
    exit_code = server.run()
    
    # Verify exit code is 0 (success)
    assert exit_code == 0, f"Expected exit code 0, got {exit_code}"
    
    # Verify we got a response for each request
    output_stream.seek(0)
    responses = [json.loads(line) for line in output_stream.readlines()]
    
    assert len(responses) == len(requests), \
        f"Expected {len(requests)} responses, got {len(responses)}"
    
    # Verify each response has the correct request ID
    for i, (request, response) in enumerate(zip(requests, responses)):
        assert response['id'] == request['id'], \
            f"Response {i} has wrong ID: expected {request['id']}, got {response['id']}"


# Feature: gemini-mcp-server-fix, Property 9: Request ordering
@given(
    num_requests=st.integers(min_value=2, max_value=20)
)
@settings(max_examples=100, deadline=None)
def test_request_ordering_property(num_requests):
    """
    Property 9: Request ordering
    
    For any sequence of requests received on stdin, the server should
    process them in FIFO order and return responses in the same order.
    
    Validates: Requirements 1.2, 6.4
    """
    # Create mock configuration
    config = ServerConfig()
    
    # Create requests with sequential IDs
    requests = [
        {
            'jsonrpc': '2.0',
            'method': 'tools/list',
            'params': {},
            'id': i
        }
        for i in range(num_requests)
    ]
    
    # Create input stream with requests
    input_lines = [json.dumps(req) + '\n' for req in requests]
    input_stream = io.StringIO(''.join(input_lines))
    
    # Create output stream to capture responses
    output_stream = io.StringIO()
    
    # Create server with mocked components
    server = MCPServer(config)
    
    # Replace stdio handler with one using our test streams
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Mock the request router to return responses with request IDs
    def mock_handle_request(request):
        return {
            'jsonrpc': '2.0',
            'result': {'request_id': request.get('id')},
            'id': request.get('id')
        }
    
    server.request_router.handle_request = mock_handle_request
    
    # Run the server
    exit_code = server.run()
    
    # Verify exit code is 0
    assert exit_code == 0
    
    # Verify responses are in order
    output_stream.seek(0)
    responses = [json.loads(line) for line in output_stream.readlines()]
    
    assert len(responses) == num_requests
    
    # Check that response IDs are in sequential order
    response_ids = [resp['id'] for resp in responses]
    expected_ids = list(range(num_requests))
    
    assert response_ids == expected_ids, \
        f"Responses out of order: expected {expected_ids}, got {response_ids}"


# Feature: gemini-mcp-server-fix, Property 13: State isolation
@given(
    num_requests=st.integers(min_value=3, max_value=10)
)
@settings(max_examples=100, deadline=None)
def test_state_isolation_property(num_requests):
    """
    Property 13: State isolation
    
    For any sequence of requests, processing one request should not
    corrupt the state needed for processing subsequent requests.
    
    Validates: Requirements 6.2
    """
    # Create mock configuration
    config = ServerConfig()
    
    # Create requests with different methods and params
    requests = []
    for i in range(num_requests):
        method = ['initialize', 'tools/list', 'tools/call'][i % 3]
        requests.append({
            'jsonrpc': '2.0',
            'method': method,
            'params': {'index': i},
            'id': i
        })
    
    # Create input stream
    input_lines = [json.dumps(req) + '\n' for req in requests]
    input_stream = io.StringIO(''.join(input_lines))
    
    # Create output stream
    output_stream = io.StringIO()
    
    # Create server
    server = MCPServer(config)
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Track state across requests
    request_count = [0]
    
    def mock_handle_request(request):
        # Increment counter to track state
        request_count[0] += 1
        
        # Return response that includes the counter
        return {
            'jsonrpc': '2.0',
            'result': {
                'request_index': request['params']['index'],
                'request_count': request_count[0]
            },
            'id': request.get('id')
        }
    
    server.request_router.handle_request = mock_handle_request
    
    # Run the server
    exit_code = server.run()
    
    # Verify exit code
    assert exit_code == 0
    
    # Verify each request was processed independently
    output_stream.seek(0)
    responses = [json.loads(line) for line in output_stream.readlines()]
    
    assert len(responses) == num_requests
    
    # Verify state was maintained correctly across all requests
    for i, response in enumerate(responses):
        assert response['result']['request_index'] == i, \
            f"Request {i} has wrong index in response"
        assert response['result']['request_count'] == i + 1, \
            f"Request {i} has wrong count: expected {i + 1}, got {response['result']['request_count']}"


# ============================================================================
# Unit Tests
# ============================================================================

def test_server_initialization():
    """Test server initialization with all components"""
    config = ServerConfig()
    server = MCPServer(config)
    
    # Verify all components are initialized
    assert server.config == config
    assert isinstance(server.oauth_manager, OAuthManager)
    assert isinstance(server.socket_handler, SocketHandler)
    assert isinstance(server.request_router, RequestRouter)
    assert isinstance(server.stdio_handler, StdioHandler)
    assert server._shutdown_requested == False


def test_graceful_shutdown_on_eof():
    """Test that server shuts down gracefully when stdin closes"""
    config = ServerConfig()
    
    # Create empty input stream (immediate EOF)
    input_stream = io.StringIO('')
    output_stream = io.StringIO()
    
    # Create server
    server = MCPServer(config)
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Run server (should exit immediately on EOF)
    exit_code = server.run()
    
    # Verify clean exit
    assert exit_code == 0
    assert server._shutdown_requested == True


def test_signal_handling():
    """Test signal handling for graceful shutdown"""
    config = ServerConfig()
    server = MCPServer(config)
    
    # Set up signal handlers
    server._setup_signal_handlers()
    
    # Verify shutdown flag is initially False
    assert server._shutdown_requested == False
    
    # Simulate SIGTERM
    os.kill(os.getpid(), signal.SIGTERM)
    
    # Give signal handler time to execute
    time.sleep(0.1)
    
    # Verify shutdown was requested
    assert server._shutdown_requested == True


def test_multiple_request_processing():
    """Test processing multiple requests in sequence"""
    config = ServerConfig()
    
    # Create multiple requests
    requests = [
        {'jsonrpc': '2.0', 'method': 'initialize', 'params': {}, 'id': 1},
        {'jsonrpc': '2.0', 'method': 'tools/list', 'params': {}, 'id': 2},
        {'jsonrpc': '2.0', 'method': 'tools/call', 'params': {'name': 'test'}, 'id': 3}
    ]
    
    # Create input stream
    input_lines = [json.dumps(req) + '\n' for req in requests]
    input_stream = io.StringIO(''.join(input_lines))
    output_stream = io.StringIO()
    
    # Create server
    server = MCPServer(config)
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Mock request router
    def mock_handle_request(request):
        return {
            'jsonrpc': '2.0',
            'result': {'method': request['method']},
            'id': request['id']
        }
    
    server.request_router.handle_request = mock_handle_request
    
    # Run server
    exit_code = server.run()
    
    # Verify success
    assert exit_code == 0
    
    # Verify all requests were processed
    output_stream.seek(0)
    responses = [json.loads(line) for line in output_stream.readlines()]
    
    assert len(responses) == 3
    assert responses[0]['id'] == 1
    assert responses[1]['id'] == 2
    assert responses[2]['id'] == 3


def test_error_recovery():
    """Test that server continues after non-fatal errors"""
    config = ServerConfig()
    
    # Create requests including one that will cause an error
    requests = [
        {'jsonrpc': '2.0', 'method': 'initialize', 'params': {}, 'id': 1},
        {'invalid': 'request'},  # This will cause a parse/validation error
        {'jsonrpc': '2.0', 'method': 'tools/list', 'params': {}, 'id': 3}
    ]
    
    # Create input stream
    input_lines = [json.dumps(req) + '\n' for req in requests]
    input_stream = io.StringIO(''.join(input_lines))
    output_stream = io.StringIO()
    
    # Create server
    server = MCPServer(config)
    server.stdio_handler = StdioHandler(input_stream, output_stream)
    
    # Mock request router
    call_count = [0]
    
    def mock_handle_request(request):
        call_count[0] += 1
        if 'invalid' in request:
            raise ValueError("Invalid request")
        return {
            'jsonrpc': '2.0',
            'result': {'ok': True},
            'id': request.get('id')
        }
    
    server.request_router.handle_request = mock_handle_request
    
    # Run server
    exit_code = server.run()
    
    # Verify server continued after error
    assert exit_code == 0
    
    # Verify we got responses (including error response)
    output_stream.seek(0)
    responses = [json.loads(line) for line in output_stream.readlines()]
    
    # Should have 3 responses (2 success + 1 error)
    assert len(responses) == 3


def test_shutdown_cleanup():
    """Test that shutdown properly cleans up resources"""
    config = ServerConfig()
    server = MCPServer(config)
    
    # Mock socket handler
    server.socket_handler.close = Mock()
    
    # Call shutdown
    server.shutdown()
    
    # Verify cleanup was performed
    assert server._shutdown_requested == True
    server.socket_handler.close.assert_called_once()
    
    # Verify shutdown is idempotent
    server.socket_handler.close.reset_mock()
    server.shutdown()
    server.socket_handler.close.assert_not_called()


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
