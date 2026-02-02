#!/usr/bin/env python3

"""
Tests for StdioHandler class

This module contains both property-based tests and unit tests for the StdioHandler
class that manages stdio communication with Gemini CLI.
"""

import json
import io
from typing import Dict, Any
from hypothesis import given, strategies as st, settings, assume
import pytest

# Import the StdioHandler class
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))
from gemini_mcp_server import StdioHandler


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 4: Stdout flushing and line formatting
@given(
    response=st.dictionaries(
        keys=st.text(min_size=1, max_size=50),
        values=st.one_of(
            st.text(min_size=0, max_size=1000),
            st.integers(),
            st.floats(allow_nan=False, allow_infinity=False),
            st.booleans(),
            st.none()
        ),
        min_size=1,
        max_size=20
    )
)
@settings(max_examples=100, deadline=3000)
def test_stdout_flushing_and_line_formatting_property(response: Dict[str, Any]):
    """
    Property 4: Stdout flushing and line formatting
    
    For any JSON-RPC response written to stdout, the response should be 
    formatted as a single line ending with a newline character, and stdout 
    should be flushed immediately.
    
    Validates: Requirements 1.3, 7.1, 7.3
    """
    # Create a mock output stream
    output_stream = io.StringIO()
    
    # Create handler with mock stream
    handler = StdioHandler(output_stream=output_stream)
    
    # Write the response
    handler.write_response(response)
    
    # Get the written output
    output = output_stream.getvalue()
    
    # Verify output ends with newline
    assert output.endswith('\n'), "Response must end with newline"
    
    # Verify output is a single line (no internal newlines)
    lines = output.split('\n')
    assert len(lines) == 2, f"Response must be single line (got {len(lines)-1} lines)"
    assert lines[1] == '', "Should only have trailing newline"
    
    # Verify the line contains valid JSON
    json_line = lines[0]
    parsed = json.loads(json_line)
    
    # Verify parsed JSON matches original response
    assert parsed == response, "Parsed response must match original"
    
    # Note: We can't directly test flushing in a unit test, but we verify
    # that the handler calls flush() by checking the implementation.
    # In real usage, the flush ensures immediate delivery to the client.


# Feature: gemini-mcp-server-fix, Property 12: Compact JSON formatting
@given(
    response=st.dictionaries(
        keys=st.text(min_size=1, max_size=50),
        values=st.one_of(
            st.text(min_size=0, max_size=500),
            st.integers(),
            st.floats(allow_nan=False, allow_infinity=False),
            st.booleans(),
            st.none(),
            st.lists(st.integers(), min_size=0, max_size=10)
        ),
        min_size=1,
        max_size=15
    )
)
@settings(max_examples=100, deadline=3000)
def test_compact_json_formatting_property(response: Dict[str, Any]):
    """
    Property 12: Compact JSON formatting
    
    For any JSON-RPC response, the JSON should be formatted compactly 
    without extra whitespace.
    
    Validates: Requirements 7.5
    """
    # Create a mock output stream
    output_stream = io.StringIO()
    
    # Create handler with mock stream
    handler = StdioHandler(output_stream=output_stream)
    
    # Write the response
    handler.write_response(response)
    
    # Get the written output (strip trailing newline)
    output = output_stream.getvalue().rstrip('\n')
    
    # Verify no extra spaces after colons (compact format uses ':' not ': ')
    # In compact JSON, we should have ',' and ':' without spaces
    assert ', ' not in output or ': ' not in output, \
        "Compact JSON should not have spaces after separators"
    
    # Verify the output matches compact JSON format
    compact_json = json.dumps(response, separators=(',', ':'))
    assert output == compact_json, \
        f"Output should match compact JSON format"
    
    # Verify no unnecessary whitespace (no multiple spaces, tabs, etc.)
    assert '  ' not in output, "Should not contain multiple consecutive spaces"
    assert '\t' not in output, "Should not contain tabs"
    
    # Verify it's still valid JSON
    parsed = json.loads(output)
    assert parsed == response


# ============================================================================
# Unit Tests
# ============================================================================

class TestStdioHandlerReading:
    """Unit tests for StdioHandler reading functionality"""
    
    def test_reading_multiple_requests_from_stdin(self):
        """Test reading multiple requests from stdin"""
        # Create mock input with multiple requests
        requests = [
            {'jsonrpc': '2.0', 'method': 'initialize', 'id': 1, 'params': {}},
            {'jsonrpc': '2.0', 'method': 'tools/list', 'id': 2, 'params': {}},
            {'jsonrpc': '2.0', 'method': 'tools/call', 'id': 3, 'params': {'name': 'test'}}
        ]
        
        input_lines = '\n'.join(json.dumps(req) for req in requests) + '\n'
        input_stream = io.StringIO(input_lines)
        
        # Create handler with mock stream
        handler = StdioHandler(input_stream=input_stream)
        
        # Read all requests
        read_requests = []
        for _ in range(len(requests)):
            req = handler.read_request()
            if req is not None:
                read_requests.append(req)
        
        # Verify all requests were read correctly
        assert len(read_requests) == len(requests)
        for i, req in enumerate(read_requests):
            assert req == requests[i]
    
    def test_eof_handling_and_graceful_shutdown(self):
        """Test EOF handling and graceful shutdown"""
        # Create mock input that ends immediately (EOF)
        input_stream = io.StringIO('')
        
        # Create handler with mock stream
        handler = StdioHandler(input_stream=input_stream)
        
        # Read should return None on EOF
        result = handler.read_request()
        assert result is None
        
        # EOF flag should be set
        assert handler.is_eof()
        
        # Subsequent reads should also return None
        result = handler.read_request()
        assert result is None
    
    def test_empty_line_handling(self):
        """Test that empty lines are skipped"""
        # Create mock input with empty lines
        input_lines = '\n\n{"jsonrpc":"2.0","method":"test","id":1}\n\n'
        input_stream = io.StringIO(input_lines)
        
        # Create handler with mock stream
        handler = StdioHandler(input_stream=input_stream)
        
        # Read should skip empty lines and return the request
        req = handler.read_request()
        assert req is not None
        assert req['method'] == 'test'
        assert req['id'] == 1
    
    def test_json_parse_error(self):
        """Test handling of invalid JSON"""
        # Create mock input with invalid JSON
        input_stream = io.StringIO('invalid json\n')
        
        # Create handler with mock stream
        handler = StdioHandler(input_stream=input_stream)
        
        # Should raise JSONDecodeError
        with pytest.raises(json.JSONDecodeError):
            handler.read_request()


class TestStdioHandlerWriting:
    """Unit tests for StdioHandler writing functionality"""
    
    def test_response_formatting(self):
        """Test response formatting"""
        output_stream = io.StringIO()
        handler = StdioHandler(output_stream=output_stream)
        
        response = {
            'jsonrpc': '2.0',
            'result': {'status': 'ok'},
            'id': 1
        }
        
        handler.write_response(response)
        
        output = output_stream.getvalue()
        
        # Should end with newline
        assert output.endswith('\n')
        
        # Should be single line
        lines = output.strip().split('\n')
        assert len(lines) == 1
        
        # Should be valid JSON
        parsed = json.loads(lines[0])
        assert parsed == response
    
    def test_stdout_flushing(self):
        """Test stdout flushing"""
        # We can't directly test flushing, but we can verify the method is called
        # by using a mock that tracks flush calls
        class FlushTrackingStream(io.StringIO):
            def __init__(self):
                super().__init__()
                self.flush_count = 0
            
            def flush(self):
                self.flush_count += 1
                super().flush()
        
        output_stream = FlushTrackingStream()
        handler = StdioHandler(output_stream=output_stream)
        
        response = {'jsonrpc': '2.0', 'result': {}, 'id': 1}
        handler.write_response(response)
        
        # Verify flush was called
        assert output_stream.flush_count > 0, "flush() should be called after writing"
    
    def test_error_response_writing(self):
        """Test writing error responses"""
        output_stream = io.StringIO()
        handler = StdioHandler(output_stream=output_stream)
        
        handler.write_error(-32700, 'Parse error', request_id=1)
        
        output = output_stream.getvalue()
        parsed = json.loads(output.strip())
        
        assert parsed['jsonrpc'] == '2.0'
        assert 'error' in parsed
        assert parsed['error']['code'] == -32700
        assert parsed['error']['message'] == 'Parse error'
        assert parsed['id'] == 1
    
    def test_error_response_with_none_id(self):
        """Test writing error responses with None ID (for parse errors)"""
        output_stream = io.StringIO()
        handler = StdioHandler(output_stream=output_stream)
        
        handler.write_error(-32700, 'Parse error', request_id=None)
        
        output = output_stream.getvalue()
        parsed = json.loads(output.strip())
        
        assert parsed['id'] is None


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
