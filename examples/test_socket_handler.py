#!/usr/bin/env python3

"""
Tests for SocketHandler class

This module contains both property-based tests and unit tests for the SocketHandler
class that manages TCP socket communication with ErlVectorDB.
"""

import json
import socket
import threading
import time
from typing import Dict, Any
from hypothesis import given, strategies as st, settings
import pytest

# Import the SocketHandler class
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))
from gemini_mcp_server import SocketHandler, ConnectionState


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 7: Connection resilience
@given(
    num_disconnects=st.integers(min_value=1, max_value=3),
    message=st.dictionaries(
        keys=st.text(min_size=1, max_size=20),
        values=st.one_of(st.text(max_size=100), st.integers(), st.booleans()),
        min_size=1,
        max_size=5
    )
)
@settings(max_examples=100, deadline=10000)
def test_connection_resilience_property(num_disconnects: int, message: Dict[str, Any]):
    """
    Property 7: Connection resilience
    
    For any connection loss to ErlVectorDB, the server should attempt to 
    reconnect before processing the next request.
    
    Validates: Requirements 3.3, 5.5
    """
    # Create a mock server that will disconnect and reconnect
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('localhost', 0))
    server_socket.listen(5)
    port = server_socket.getsockname()[1]
    
    message_bytes = json.dumps(message).encode('utf-8')
    disconnect_count = [0]
    test_complete = threading.Event()
    
    def mock_server():
        """Mock server that disconnects and accepts reconnections"""
        try:
            for i in range(num_disconnects + 1):
                # Accept connection
                client_socket, _ = server_socket.accept()
                
                if i < num_disconnects:
                    # Disconnect after a short time
                    time.sleep(0.05)
                    client_socket.close()
                    disconnect_count[0] += 1
                else:
                    # Final connection - send the message
                    client_socket.sendall(message_bytes)
                    time.sleep(0.1)
                    client_socket.close()
            
            test_complete.set()
        except Exception as e:
            print(f"Mock server error: {e}")
        finally:
            server_socket.close()
    
    server_thread = threading.Thread(target=mock_server, daemon=True)
    server_thread.start()
    
    try:
        # Create socket handler with reconnection enabled
        handler = SocketHandler('localhost', port, timeout=2, buffer_size=1024,
                               max_reconnect_attempts=num_disconnects + 1,
                               reconnect_delay=0.1)
        
        # Initial connection
        handler.connect()
        
        # Simulate connection loss and reconnection attempts
        for i in range(num_disconnects):
            # Wait for server to disconnect
            time.sleep(0.1)
            
            # Check connection health (should detect disconnection)
            is_healthy = handler.check_connection_health()
            
            # Try to reconnect
            if not is_healthy:
                reconnected = handler.reconnect()
                assert reconnected, f"Failed to reconnect on attempt {i+1}"
        
        # After all reconnections, should be able to receive message
        received = handler.receive_message()
        assert received == message, "Message not received correctly after reconnections"
        
        handler.close()
        test_complete.wait(timeout=2)
        
    except Exception as e:
        pytest.fail(f"Property test failed: {e}")
    finally:
        server_thread.join(timeout=2)


# Feature: gemini-mcp-server-fix, Property 5: Complete message reading
@given(
    message=st.dictionaries(
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
@settings(max_examples=100, deadline=5000)
def test_complete_message_reading_property(message: Dict[str, Any]):
    """
    Property 5: Complete message reading
    
    For any response from ErlVectorDB (regardless of size), the server should 
    read the complete message before attempting to parse it as JSON.
    
    Validates: Requirements 5.2, 5.3
    """
    # Create a mock server that sends the message
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('localhost', 0))
    server_socket.listen(1)
    port = server_socket.getsockname()[1]
    
    # Serialize the message
    message_bytes = json.dumps(message).encode('utf-8')
    
    # Flag to track if server sent complete message
    server_sent_complete = threading.Event()
    received_message = [None]
    
    def mock_server():
        """Mock server that sends the message in chunks"""
        try:
            client_socket, _ = server_socket.accept()
            
            # Send message in chunks to simulate partial reads
            chunk_size = max(1, len(message_bytes) // 5)  # Send in ~5 chunks
            offset = 0
            
            while offset < len(message_bytes):
                end = min(offset + chunk_size, len(message_bytes))
                client_socket.send(message_bytes[offset:end])
                offset = end
                time.sleep(0.01)  # Small delay between chunks
            
            server_sent_complete.set()
            client_socket.close()
        except Exception as e:
            print(f"Mock server error: {e}")
        finally:
            server_socket.close()
    
    # Start mock server in background
    server_thread = threading.Thread(target=mock_server, daemon=True)
    server_thread.start()
    
    try:
        # Create socket handler and connect
        handler = SocketHandler('localhost', port, timeout=5, buffer_size=16)
        handler.connect()
        
        # Receive the message
        received_message[0] = handler.receive_message()
        
        # Wait for server to finish sending
        server_sent_complete.wait(timeout=2)
        
        # Verify the complete message was received correctly
        assert received_message[0] == message, \
            f"Received message does not match sent message"
        
        handler.close()
        
    except Exception as e:
        pytest.fail(f"Property test failed: {e}")
    finally:
        server_thread.join(timeout=1)


# ============================================================================
# Unit Tests
# ============================================================================

class TestSocketHandlerConnection:
    """Unit tests for SocketHandler connection management"""
    
    def test_connection_establishment(self):
        """Test that socket handler can establish a connection"""
        # Create a mock server
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(1)
        port = server_socket.getsockname()[1]
        
        def mock_server():
            client_socket, _ = server_socket.accept()
            time.sleep(0.5)  # Keep connection open briefly
            client_socket.close()
            server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        
        try:
            # Test connection
            handler = SocketHandler('localhost', port, timeout=5)
            
            # Initially disconnected
            assert handler.get_state() == ConnectionState.DISCONNECTED
            assert not handler.is_connected()
            
            # Connect
            handler.connect()
            
            # Should be connected
            assert handler.get_state() == ConnectionState.CONNECTED
            assert handler.is_connected()
            
            # Clean up
            handler.close()
            assert handler.get_state() == ConnectionState.DISCONNECTED
            
        finally:
            server_thread.join(timeout=2)
    
    def test_large_message_handling(self):
        """Test handling of messages larger than buffer size (> 8KB)"""
        # Create a large message (20KB)
        large_data = {
            'vectors': [[float(i) for i in range(1000)] for _ in range(20)],
            'metadata': {'test': 'large_message'}
        }
        message_bytes = json.dumps(large_data).encode('utf-8')
        assert len(message_bytes) > 8192, "Test message should be > 8KB"
        
        # Create mock server
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(1)
        port = server_socket.getsockname()[1]
        
        def mock_server():
            client_socket, _ = server_socket.accept()
            # Send the large message
            client_socket.sendall(message_bytes)
            time.sleep(0.1)
            client_socket.close()
            server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        
        try:
            # Connect and receive
            handler = SocketHandler('localhost', port, timeout=5, buffer_size=4096)
            handler.connect()
            
            received = handler.receive_message()
            
            # Verify complete message received
            assert received == large_data
            assert received['metadata']['test'] == 'large_message'
            assert len(received['vectors']) == 20
            
            handler.close()
            
        finally:
            server_thread.join(timeout=2)
    
    def test_connection_timeout(self):
        """Test that connection timeout is handled properly"""
        # Try to connect to a non-existent server with short timeout
        handler = SocketHandler('192.0.2.1', 9999, timeout=1)  # 192.0.2.1 is TEST-NET-1
        
        with pytest.raises(ConnectionError) as exc_info:
            handler.connect()
        
        assert 'timeout' in str(exc_info.value).lower() or 'failed to connect' in str(exc_info.value).lower()
        assert handler.get_state() == ConnectionState.ERROR
    
    def test_send_without_connection(self):
        """Test that sending without connection raises error"""
        handler = SocketHandler('localhost', 8080)
        
        with pytest.raises(ConnectionError) as exc_info:
            handler.send_message({'test': 'message'})
        
        assert 'not connected' in str(exc_info.value).lower()
    
    def test_receive_without_connection(self):
        """Test that receiving without connection raises error"""
        handler = SocketHandler('localhost', 8080)
        
        with pytest.raises(ConnectionError) as exc_info:
            handler.receive_message()
        
        assert 'not connected' in str(exc_info.value).lower()


class TestConnectionResilience:
    """Unit tests for connection resilience features"""
    
    def test_reconnection_after_connection_loss(self):
        """Test that handler can reconnect after connection loss"""
        # Create a mock server that accepts multiple connections
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(5)
        port = server_socket.getsockname()[1]
        
        connection_count = [0]
        first_connected = threading.Event()
        second_connected = threading.Event()
        
        def mock_server():
            try:
                # Accept first connection
                client_socket, _ = server_socket.accept()
                connection_count[0] += 1
                first_connected.set()
                time.sleep(0.15)
                # Close it to simulate connection loss
                client_socket.close()
                
                # Accept second connection (reconnection)
                client_socket, _ = server_socket.accept()
                connection_count[0] += 1
                second_connected.set()
                time.sleep(0.2)
                client_socket.close()
            finally:
                server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        
        try:
            # Create handler and connect
            handler = SocketHandler('localhost', port, timeout=5, 
                                   max_reconnect_attempts=3, reconnect_delay=0.1)
            handler.connect()
            
            # Wait for server to accept connection
            first_connected.wait(timeout=2)
            
            assert handler.is_connected()
            assert connection_count[0] == 1
            
            # Wait for server to close connection
            time.sleep(0.25)
            
            # Check health (should detect disconnection)
            is_healthy = handler.check_connection_health()
            assert not is_healthy, "Should detect connection loss"
            
            # Reconnect
            reconnected = handler.reconnect()
            
            # Wait for server to accept reconnection
            second_connected.wait(timeout=2)
            
            assert reconnected, "Should successfully reconnect"
            assert handler.is_connected()
            assert connection_count[0] == 2
            
            handler.close()
            
        finally:
            server_thread.join(timeout=2)
    
    def test_timeout_handling(self):
        """Test that timeout is handled properly during receive"""
        # Create a mock server that accepts connection but never sends data
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(1)
        port = server_socket.getsockname()[1]
        
        def mock_server():
            client_socket, _ = server_socket.accept()
            # Don't send anything, just wait
            time.sleep(5)
            client_socket.close()
            server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        
        try:
            # Create handler with short timeout
            handler = SocketHandler('localhost', port, timeout=1)
            handler.connect()
            
            # Try to receive (should timeout)
            with pytest.raises(ConnectionError) as exc_info:
                handler.receive_message()
            
            assert 'timeout' in str(exc_info.value).lower()
            assert handler.get_state() == ConnectionState.ERROR
            
            handler.close()
            
        finally:
            server_thread.join(timeout=6)
    
    def test_error_response_on_connection_failure(self):
        """Test that connection failure results in appropriate error"""
        # Try to connect to non-existent server
        handler = SocketHandler('localhost', 19999, timeout=1,
                               max_reconnect_attempts=2, reconnect_delay=0.1)
        
        # Connection should fail
        with pytest.raises(ConnectionError):
            handler.connect()
        
        # Reconnection should also fail
        reconnected = handler.reconnect()
        assert not reconnected, "Should not reconnect to non-existent server"
        assert handler.get_state() == ConnectionState.ERROR
    
    def test_connection_health_check(self):
        """Test connection health checking"""
        # Create a mock server
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('localhost', 0))
        server_socket.listen(1)
        port = server_socket.getsockname()[1]
        
        def mock_server():
            client_socket, _ = server_socket.accept()
            time.sleep(0.3)
            client_socket.close()
            server_socket.close()
        
        server_thread = threading.Thread(target=mock_server, daemon=True)
        server_thread.start()
        
        try:
            handler = SocketHandler('localhost', port, timeout=5)
            
            # Before connection
            assert not handler.check_connection_health()
            
            # After connection
            handler.connect()
            assert handler.check_connection_health()
            
            # After close
            handler.close()
            assert not handler.check_connection_health()
            
        finally:
            server_thread.join(timeout=2)
    
    def test_connection_stats(self):
        """Test connection statistics tracking"""
        handler = SocketHandler('localhost', 8080, timeout=10,
                               max_reconnect_attempts=3, reconnect_delay=1.0)
        
        stats = handler.get_connection_stats()
        
        assert stats['state'] == 'disconnected'
        assert stats['connected'] == False
        assert stats['connection_lost_count'] == 0
        assert stats['host'] == 'localhost'
        assert stats['port'] == 8080
        assert stats['timeout'] == 10


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
