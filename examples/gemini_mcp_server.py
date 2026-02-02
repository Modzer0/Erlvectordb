#!/usr/bin/env python3

"""
ErlVectorDB MCP Server for Gemini CLI

This script acts as an MCP server that Gemini CLI can connect to via stdio.
It forwards requests to the ErlVectorDB MCP server and adds AI capabilities.

Usage:
    python3 gemini_mcp_server.py

Environment Variables:
    GEMINI_API_KEY - Your Google AI API key (optional, for AI features)
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
import sys
import os
import logging
import time
from typing import Dict, Any, Optional
from dataclasses import dataclass, field
from enum import Enum

# Set up logging to stderr (stdout is used for MCP protocol)
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    stream=sys.stderr
)
logger = logging.getLogger(__name__)


class ConnectionState(Enum):
    """Connection state enumeration"""
    DISCONNECTED = "disconnected"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    ERROR = "error"


class SocketHandler:
    """Handles TCP socket communication with ErlVectorDB MCP server"""
    
    def __init__(self, host: str, port: int, timeout: int = 30, buffer_size: int = 8192,
                 max_reconnect_attempts: int = 3, reconnect_delay: float = 1.0):
        """
        Initialize socket handler
        
        Args:
            host: Target host address
            port: Target port number
            timeout: Socket timeout in seconds
            buffer_size: Initial buffer size for reading
            max_reconnect_attempts: Maximum number of reconnection attempts
            reconnect_delay: Initial delay between reconnection attempts (seconds)
        """
        self.host = host
        self.port = port
        self.timeout = timeout
        self.buffer_size = buffer_size
        self.max_reconnect_attempts = max_reconnect_attempts
        self.reconnect_delay = reconnect_delay
        self.socket: Optional[socket.socket] = None
        self.state = ConnectionState.DISCONNECTED
        self._read_buffer = b""
        self._connection_lost_count = 0
        
        logger.debug(f"SocketHandler initialized for {host}:{port}")
    
    def connect(self) -> None:
        """
        Establish TCP connection to the server
        
        Raises:
            ConnectionError: If connection fails
        """
        if self.state == ConnectionState.CONNECTED:
            logger.debug("Already connected")
            return
        
        try:
            self.state = ConnectionState.CONNECTING
            logger.info(f"Connecting to {self.host}:{self.port}")
            
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(self.timeout)
            self.socket.connect((self.host, self.port))
            
            self.state = ConnectionState.CONNECTED
            self._read_buffer = b""
            logger.info(f"Connected to {self.host}:{self.port}")
            
        except socket.timeout as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Connection timeout: {e}")
            raise ConnectionError(f"Connection timeout to {self.host}:{self.port}") from e
        except socket.error as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Connection error: {e}")
            raise ConnectionError(f"Failed to connect to {self.host}:{self.port}: {e}") from e
        except Exception as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Unexpected error during connection: {e}")
            raise ConnectionError(f"Unexpected connection error: {e}") from e
    
    def send_message(self, message: Dict) -> None:
        """
        Send a JSON message over the socket
        
        Args:
            message: Dictionary to send as JSON
            
        Raises:
            ConnectionError: If not connected or send fails
        """
        if not self.is_connected():
            raise ConnectionError("Not connected to server")
        
        try:
            # Encode message as UTF-8 JSON
            message_bytes = json.dumps(message).encode('utf-8')
            logger.debug(f"Sending message: {len(message_bytes)} bytes")
            
            # Send all bytes
            self.socket.sendall(message_bytes)
            
        except socket.timeout as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Send timeout: {e}")
            raise ConnectionError("Send timeout") from e
        except socket.error as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Send error: {e}")
            raise ConnectionError(f"Failed to send message: {e}") from e
        except Exception as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Unexpected error during send: {e}")
            raise ConnectionError(f"Unexpected send error: {e}") from e
    
    def receive_message(self) -> Dict:
        """
        Receive a complete JSON message from the socket
        
        Uses buffered reading with JSON parsing to detect complete messages.
        Supports messages larger than the initial buffer size.
        
        Returns:
            Parsed JSON message as dictionary
            
        Raises:
            ConnectionError: If not connected or receive fails
            json.JSONDecodeError: If message is not valid JSON
        """
        if not self.is_connected():
            raise ConnectionError("Not connected to server")
        
        try:
            # Keep reading until we have a complete JSON message
            # This handles messages that are larger than the buffer size
            # and messages that arrive in multiple TCP packets
            while True:
                # Try to parse what we have in the buffer
                if self._read_buffer:
                    try:
                        # Attempt to decode and parse JSON
                        # This will succeed only if we have a complete JSON object
                        message_str = self._read_buffer.decode('utf-8')
                        message = json.loads(message_str)
                        
                        # Success! Clear buffer and return
                        # The buffer contained exactly one complete JSON message
                        logger.debug(f"Received complete message: {len(self._read_buffer)} bytes")
                        self._read_buffer = b""
                        return message
                        
                    except json.JSONDecodeError:
                        # Incomplete JSON, need to read more
                        # This is expected when we have a partial message
                        logger.debug(f"Incomplete JSON in buffer ({len(self._read_buffer)} bytes), reading more...")
                    except UnicodeDecodeError:
                        # Incomplete UTF-8, need to read more
                        # This can happen if a multi-byte UTF-8 character is split across packets
                        logger.debug(f"Incomplete UTF-8 in buffer ({len(self._read_buffer)} bytes), reading more...")
                
                # Read more data from socket
                # We need more bytes to complete the message
                try:
                    chunk = self.socket.recv(self.buffer_size)
                    
                    if not chunk:
                        # Connection closed by remote
                        # recv() returns empty bytes when the connection is closed
                        self.state = ConnectionState.DISCONNECTED
                        raise ConnectionError("Connection closed by remote host")
                    
                    # Append new data to buffer and try parsing again
                    self._read_buffer += chunk
                    logger.debug(f"Read {len(chunk)} bytes, buffer now {len(self._read_buffer)} bytes")
                    
                except socket.timeout as e:
                    self.state = ConnectionState.ERROR
                    logger.error(f"Receive timeout: {e}")
                    raise ConnectionError("Receive timeout") from e
                    
        except ConnectionError:
            # Re-raise connection errors as-is
            raise
        except json.JSONDecodeError as e:
            # Re-raise JSON errors as-is
            logger.error(f"JSON decode error: {e}")
            raise
        except socket.error as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Receive error: {e}")
            raise ConnectionError(f"Failed to receive message: {e}") from e
        except Exception as e:
            self.state = ConnectionState.ERROR
            logger.error(f"Unexpected error during receive: {e}")
            raise ConnectionError(f"Unexpected receive error: {e}") from e
    
    def close(self) -> None:
        """Close the socket connection"""
        if self.socket:
            try:
                logger.info(f"Closing connection to {self.host}:{self.port}")
                self.socket.close()
            except Exception as e:
                logger.warning(f"Error closing socket: {e}")
            finally:
                self.socket = None
                self.state = ConnectionState.DISCONNECTED
                self._read_buffer = b""
    
    def is_connected(self) -> bool:
        """
        Check if socket is connected
        
        Returns:
            True if connected, False otherwise
        """
        return self.state == ConnectionState.CONNECTED and self.socket is not None
    
    def get_state(self) -> ConnectionState:
        """
        Get current connection state
        
        Returns:
            Current ConnectionState
        """
        return self.state
    
    def check_connection_health(self) -> bool:
        """
        Check if the connection is healthy
        
        Performs a lightweight check to detect connection loss.
        
        Returns:
            True if connection is healthy, False if connection is lost
        """
        if not self.socket or self.state != ConnectionState.CONNECTED:
            logger.debug("Connection health check: not connected")
            return False
        
        try:
            # Use MSG_PEEK to check if connection is alive without consuming data
            # This will raise an error if the connection is closed
            self.socket.setblocking(False)
            try:
                data = self.socket.recv(1, socket.MSG_PEEK)
                # If recv returns empty bytes, connection is closed
                if data == b'':
                    logger.warning("Connection health check: connection closed by remote")
                    self.state = ConnectionState.DISCONNECTED
                    return False
            except BlockingIOError:
                # No data available, but connection is alive
                pass
            finally:
                self.socket.setblocking(True)
            
            logger.debug("Connection health check: healthy")
            return True
            
        except socket.error as e:
            logger.warning(f"Connection health check failed: {e}")
            self.state = ConnectionState.DISCONNECTED
            return False
        except Exception as e:
            logger.warning(f"Unexpected error in health check: {e}")
            self.state = ConnectionState.ERROR
            return False
    
    def reconnect(self) -> bool:
        """
        Attempt to reconnect to the server with retry logic
        
        Tries to reconnect up to max_reconnect_attempts times with
        exponential backoff between attempts.
        
        Returns:
            True if reconnection successful, False otherwise
        """
        logger.info(f"Attempting to reconnect to {self.host}:{self.port}")
        
        # Close existing socket if any
        # This ensures we start with a clean state
        if self.socket:
            try:
                self.socket.close()
            except Exception:
                pass
            self.socket = None
        
        self.state = ConnectionState.DISCONNECTED
        self._read_buffer = b""
        
        # Try to reconnect with exponential backoff
        # Start with initial delay, then double it each time
        delay = self.reconnect_delay
        
        for attempt in range(self.max_reconnect_attempts):
            try:
                logger.info(f"Reconnection attempt {attempt + 1}/{self.max_reconnect_attempts}")
                
                # Attempt connection
                self.connect()
                
                # Success!
                logger.info(f"Reconnection successful after {attempt + 1} attempt(s)")
                self._connection_lost_count += 1
                return True
                
            except ConnectionError as e:
                logger.warning(f"Reconnection attempt {attempt + 1} failed: {e}")
                
                # Don't sleep after the last attempt
                # No point waiting if we're giving up
                if attempt < self.max_reconnect_attempts - 1:
                    logger.info(f"Waiting {delay:.1f}s before next attempt...")
                    time.sleep(delay)
                    
                    # Exponential backoff (double the delay each time)
                    # This reduces load on the server and gives it time to recover
                    delay *= 2
        
        # All reconnection attempts failed
        logger.error(f"Failed to reconnect after {self.max_reconnect_attempts} attempts")
        self.state = ConnectionState.ERROR
        return False
    
    def send_with_reconnect(self, message: Dict) -> None:
        """
        Send a message with automatic reconnection on failure
        
        Args:
            message: Dictionary to send as JSON
            
        Raises:
            ConnectionError: If send fails and reconnection fails
        """
        try:
            self.send_message(message)
        except ConnectionError as e:
            logger.warning(f"Send failed, attempting reconnection: {e}")
            
            # Try to reconnect
            if self.reconnect():
                # Retry send after successful reconnection
                logger.info("Retrying send after reconnection")
                self.send_message(message)
            else:
                # Reconnection failed
                raise ConnectionError(f"Failed to send message: {e}, reconnection failed")
    
    def receive_with_reconnect(self) -> Dict:
        """
        Receive a message with automatic reconnection on failure
        
        Returns:
            Parsed JSON message as dictionary
            
        Raises:
            ConnectionError: If receive fails and reconnection fails
        """
        try:
            return self.receive_message()
        except ConnectionError as e:
            logger.warning(f"Receive failed, attempting reconnection: {e}")
            
            # Try to reconnect
            if self.reconnect():
                # After reconnection, we can't retry the receive
                # because we don't know what message was being sent
                # Raise an error to let the caller handle it
                raise ConnectionError(f"Connection was lost and reconnected, please retry request")
            else:
                # Reconnection failed
                raise ConnectionError(f"Failed to receive message: {e}, reconnection failed")
    
    def send_request_with_timeout(self, message: Dict, timeout_override: Optional[int] = None) -> Dict:
        """
        Send a request and receive response with timeout handling
        
        This is a convenience method that combines send and receive with
        proper timeout handling and error responses.
        
        Args:
            message: Request message to send
            timeout_override: Optional timeout override for this request
            
        Returns:
            Response message
            
        Raises:
            ConnectionError: If connection fails
        """
        # Save original timeout
        original_timeout = self.timeout
        
        try:
            # Apply timeout override if provided
            if timeout_override is not None:
                self.timeout = timeout_override
                if self.socket:
                    self.socket.settimeout(timeout_override)
            
            # Check connection health before sending
            if not self.check_connection_health():
                logger.warning("Connection unhealthy, attempting reconnection")
                if not self.reconnect():
                    raise ConnectionError("Connection lost and reconnection failed")
            
            # Send request
            self.send_message(message)
            
            # Receive response
            response = self.receive_message()
            
            return response
            
        except socket.timeout as e:
            logger.error(f"Request timeout after {self.timeout}s")
            raise ConnectionError(f"Request timeout: {e}") from e
            
        finally:
            # Restore original timeout
            self.timeout = original_timeout
            if self.socket:
                self.socket.settimeout(original_timeout)
    
    def get_connection_stats(self) -> Dict[str, Any]:
        """
        Get connection statistics
        
        Returns:
            Dictionary with connection statistics
        """
        return {
            'state': self.state.value,
            'connected': self.is_connected(),
            'connection_lost_count': self._connection_lost_count,
            'host': self.host,
            'port': self.port,
            'timeout': self.timeout
        }


@dataclass
class ServerConfig:
    """Configuration for the ErlVectorDB MCP Server"""
    
    # ErlVectorDB connection settings
    erlvectordb_host: str = 'localhost'
    erlvectordb_port: int = 8080
    
    # OAuth server settings
    oauth_host: str = 'localhost'
    oauth_port: int = 8081
    client_id: str = 'admin'
    client_secret: str = 'admin_secret_2024'
    
    # Socket settings
    socket_timeout: int = 30
    socket_buffer_size: int = 8192
    
    # Reconnection settings
    max_reconnect_attempts: int = 3
    reconnect_delay: float = 1.0
    
    # OAuth retry settings
    oauth_max_retries: int = 3
    oauth_initial_backoff: float = 1.0
    oauth_max_backoff: float = 30.0
    oauth_backoff_multiplier: float = 2.0
    
    # Logging settings
    log_level: str = 'INFO'
    
    @classmethod
    def from_environment(cls) -> 'ServerConfig':
        """Create configuration from environment variables"""
        return cls(
            erlvectordb_host=os.getenv('ERLVECTORDB_HOST', 'localhost'),
            erlvectordb_port=int(os.getenv('ERLVECTORDB_PORT', '8080')),
            oauth_host=os.getenv('ERLVECTORDB_OAUTH_HOST', 'localhost'),
            oauth_port=int(os.getenv('ERLVECTORDB_OAUTH_PORT', '8081')),
            client_id=os.getenv('ERLVECTORDB_CLIENT_ID', 'admin'),
            client_secret=os.getenv('ERLVECTORDB_CLIENT_SECRET', 'admin_secret_2024'),
            socket_timeout=int(os.getenv('ERLVECTORDB_SOCKET_TIMEOUT', '30')),
            socket_buffer_size=int(os.getenv('ERLVECTORDB_BUFFER_SIZE', '8192')),
            max_reconnect_attempts=int(os.getenv('ERLVECTORDB_MAX_RECONNECT_ATTEMPTS', '3')),
            reconnect_delay=float(os.getenv('ERLVECTORDB_RECONNECT_DELAY', '1.0')),
            oauth_max_retries=int(os.getenv('ERLVECTORDB_OAUTH_MAX_RETRIES', '3')),
            oauth_initial_backoff=float(os.getenv('ERLVECTORDB_OAUTH_INITIAL_BACKOFF', '1.0')),
            oauth_max_backoff=float(os.getenv('ERLVECTORDB_OAUTH_MAX_BACKOFF', '30.0')),
            oauth_backoff_multiplier=float(os.getenv('ERLVECTORDB_OAUTH_BACKOFF_MULTIPLIER', '2.0')),
            log_level=os.getenv('ERLVECTORDB_LOG_LEVEL', 'INFO')
        )
    
    def validate(self) -> None:
        """Validate configuration values"""
        errors = []
        
        # Validate port ranges
        if not (1 <= self.erlvectordb_port <= 65535):
            errors.append(f"Invalid erlvectordb_port: {self.erlvectordb_port} (must be 1-65535)")
        
        if not (1 <= self.oauth_port <= 65535):
            errors.append(f"Invalid oauth_port: {self.oauth_port} (must be 1-65535)")
        
        # Validate timeout
        if self.socket_timeout <= 0:
            errors.append(f"Invalid socket_timeout: {self.socket_timeout} (must be > 0)")
        
        # Validate buffer size
        if self.socket_buffer_size <= 0:
            errors.append(f"Invalid socket_buffer_size: {self.socket_buffer_size} (must be > 0)")
        
        # Validate reconnect attempts
        if self.max_reconnect_attempts < 0:
            errors.append(f"Invalid max_reconnect_attempts: {self.max_reconnect_attempts} (must be >= 0)")
        
        # Validate reconnect delay
        if self.reconnect_delay < 0:
            errors.append(f"Invalid reconnect_delay: {self.reconnect_delay} (must be >= 0)")
        
        # Validate OAuth retry settings
        if self.oauth_max_retries < 0:
            errors.append(f"Invalid oauth_max_retries: {self.oauth_max_retries} (must be >= 0)")
        
        if self.oauth_initial_backoff <= 0:
            errors.append(f"Invalid oauth_initial_backoff: {self.oauth_initial_backoff} (must be > 0)")
        
        if self.oauth_max_backoff <= 0:
            errors.append(f"Invalid oauth_max_backoff: {self.oauth_max_backoff} (must be > 0)")
        
        if self.oauth_backoff_multiplier <= 1.0:
            errors.append(f"Invalid oauth_backoff_multiplier: {self.oauth_backoff_multiplier} (must be > 1.0)")
        
        # Validate log level
        valid_log_levels = ['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL']
        if self.log_level.upper() not in valid_log_levels:
            errors.append(f"Invalid log_level: {self.log_level} (must be one of {valid_log_levels})")
        
        # Validate required string fields are not empty
        if not self.erlvectordb_host:
            errors.append("erlvectordb_host cannot be empty")
        
        if not self.oauth_host:
            errors.append("oauth_host cannot be empty")
        
        if not self.client_id:
            errors.append("client_id cannot be empty")
        
        if not self.client_secret:
            errors.append("client_secret cannot be empty")
        
        if errors:
            raise ValueError(f"Configuration validation failed: {'; '.join(errors)}")
    
    def __str__(self) -> str:
        """String representation (masks sensitive data)"""
        return (
            f"ServerConfig("
            f"erlvectordb={self.erlvectordb_host}:{self.erlvectordb_port}, "
            f"oauth={self.oauth_host}:{self.oauth_port}, "
            f"client_id={self.client_id}, "
            f"timeout={self.socket_timeout}s, "
            f"buffer={self.socket_buffer_size}B, "
            f"log_level={self.log_level})"
        )


class AuthenticationError(Exception):
    """Exception raised for authentication errors"""
    pass


class OAuthManager:
    """Manages OAuth token acquisition, caching, and refresh"""
    
    def __init__(self, config: ServerConfig):
        """
        Initialize OAuth manager
        
        Args:
            config: Server configuration containing OAuth settings
        """
        self.config = config
        self._access_token: Optional[str] = None
        self._token_expiry: Optional[float] = None
        self._token_type: str = "Bearer"
        
        logger.debug("OAuthManager initialized")
    
    def get_token(self) -> str:
        """
        Get current valid access token
        
        Returns a cached token if valid, otherwise acquires a new one.
        
        Returns:
            Valid access token
            
        Raises:
            AuthenticationError: If token acquisition fails
        """
        # Check if we have a valid cached token
        if self.is_token_valid():
            logger.debug("Using cached OAuth token")
            return self._access_token
        
        # Need to acquire a new token
        logger.info("Acquiring new OAuth token")
        return self.refresh_token()
    
    def refresh_token(self) -> str:
        """
        Force token refresh
        
        Acquires a new token from the OAuth server with retry logic
        and exponential backoff.
        
        Returns:
            New access token
            
        Raises:
            AuthenticationError: If all retry attempts fail
        """
        last_error = None
        backoff = self.config.oauth_initial_backoff
        
        # Retry loop with exponential backoff
        # This handles transient network issues and server overload
        for attempt in range(self.config.oauth_max_retries):
            try:
                logger.info(f"OAuth token request attempt {attempt + 1}/{self.config.oauth_max_retries}")
                
                # Make token request
                token_data = self._request_token()
                
                # Cache the token for future use
                # This avoids unnecessary token requests
                self._access_token = token_data['access_token']
                self._token_type = token_data.get('token_type', 'Bearer')
                
                # Calculate expiry time (with 60 second buffer for safety)
                # The buffer ensures we refresh before actual expiry
                expires_in = token_data.get('expires_in', 3600)
                self._token_expiry = time.time() + expires_in - 60
                
                logger.info(f"OAuth token acquired successfully (expires in {expires_in}s)")
                return self._access_token
                
            except requests.exceptions.RequestException as e:
                # Network or HTTP error - retry with backoff
                last_error = e
                logger.warning(f"OAuth token request failed (attempt {attempt + 1}): {e}")
                
                # Don't sleep after the last attempt
                # No point waiting if we're giving up
                if attempt < self.config.oauth_max_retries - 1:
                    logger.info(f"Retrying in {backoff:.1f}s...")
                    time.sleep(backoff)
                    
                    # Exponential backoff with max limit
                    # Prevents excessive delays while still backing off
                    backoff = min(
                        backoff * self.config.oauth_backoff_multiplier,
                        self.config.oauth_max_backoff
                    )
            
            except (KeyError, ValueError) as e:
                # Invalid response format - don't retry
                # These errors indicate a protocol issue, not a transient failure
                last_error = e
                logger.error(f"Invalid OAuth response format: {e}")
                break
        
        # All retries failed
        error_msg = f"Failed to acquire OAuth token after {self.config.oauth_max_retries} attempts"
        if last_error:
            error_msg += f": {last_error}"
        
        logger.error(error_msg)
        raise AuthenticationError(error_msg)
    
    def _request_token(self) -> Dict[str, Any]:
        """
        Make HTTP request to OAuth server for token
        
        Returns:
            Token response data
            
        Raises:
            requests.exceptions.RequestException: If request fails
            KeyError: If response is missing required fields
        """
        url = f'http://{self.config.oauth_host}:{self.config.oauth_port}/oauth/token'
        
        # Build form data as string to avoid URL encoding issues with spaces
        # The Erlang server expects spaces in scope, not + encoding
        data_string = (
            f'grant_type=client_credentials&'
            f'client_id={self.config.client_id}&'
            f'client_secret={self.config.client_secret}&'
            f'scope=read write admin'
        )
        
        headers = {'Content-Type': 'application/x-www-form-urlencoded'}
        
        logger.debug(f"Requesting OAuth token from {url}")
        
        response = requests.post(url, data=data_string, headers=headers, timeout=10)
        response.raise_for_status()
        
        token_data = response.json()
        
        # Validate required fields
        if 'access_token' not in token_data:
            raise KeyError("Response missing 'access_token' field")
        
        return token_data
    
    def is_token_valid(self) -> bool:
        """
        Check if current token is still valid
        
        Returns:
            True if token exists and hasn't expired, False otherwise
        """
        if not self._access_token:
            logger.debug("No cached token")
            return False
        
        if not self._token_expiry:
            logger.debug("No expiry time set")
            return False
        
        if time.time() >= self._token_expiry:
            logger.debug("Token expired")
            return False
        
        return True
    
    def handle_401_response(self) -> str:
        """
        Handle 401 Unauthorized response by refreshing token
        
        This should be called when a request returns 401, indicating
        the token may have been revoked or is otherwise invalid.
        
        Returns:
            New access token
            
        Raises:
            AuthenticationError: If token refresh fails
        """
        logger.warning("Received 401 Unauthorized, refreshing token")
        
        # Invalidate current token
        self._access_token = None
        self._token_expiry = None
        
        # Get new token
        return self.refresh_token()
    
    def get_auth_header(self) -> Dict[str, str]:
        """
        Get authorization header for HTTP requests
        
        Returns:
            Dictionary with Authorization header
            
        Raises:
            AuthenticationError: If token acquisition fails
        """
        token = self.get_token()
        return {
            'Authorization': f'{self._token_type} {token}'
        }
    
    def get_auth_dict(self) -> Dict[str, Any]:
        """
        Get auth dictionary for MCP requests
        
        Returns:
            Dictionary with auth type and token
            
        Raises:
            AuthenticationError: If token acquisition fails
        """
        token = self.get_token()
        return {
            'type': 'bearer',
            'token': token
        }


class StdioHandler:
    """Handles stdio communication with Gemini CLI"""
    
    def __init__(self, input_stream=None, output_stream=None):
        """
        Initialize stdio handler
        
        Args:
            input_stream: Input stream to read from (default: sys.stdin)
            output_stream: Output stream to write to (default: sys.stdout)
        """
        self.input_stream = input_stream or sys.stdin
        self.output_stream = output_stream or sys.stdout
        self._eof_reached = False
        
        logger.debug("StdioHandler initialized")
    
    def read_request(self) -> Optional[Dict]:
        """
        Read and parse one JSON-RPC request from stdin
        
        Reads line-by-line from stdin until a complete JSON request is received.
        Returns None on EOF to signal graceful shutdown.
        
        Returns:
            Parsed JSON-RPC request as dictionary, or None on EOF
            
        Raises:
            json.JSONDecodeError: If line contains invalid JSON
        """
        if self._eof_reached:
            return None
        
        try:
            # Read one line from input
            line = self.input_stream.readline()
            
            # Check for EOF
            if not line:
                logger.info("EOF detected on stdin, initiating graceful shutdown")
                self._eof_reached = True
                return None
            
            # Strip whitespace
            line = line.strip()
            
            # Skip empty lines
            if not line:
                logger.debug("Skipping empty line")
                return self.read_request()  # Recursively read next line
            
            # Parse JSON
            logger.debug(f"Read request line: {len(line)} bytes")
            request = json.loads(line)
            
            return request
            
        except json.JSONDecodeError as e:
            logger.error(f"JSON parse error: {e}")
            raise
        except Exception as e:
            logger.error(f"Error reading from stdin: {e}")
            raise
    
    def write_response(self, response: Dict) -> None:
        """
        Write JSON-RPC response to stdout with explicit flushing
        
        Formats the response as compact single-line JSON with a newline,
        then explicitly flushes stdout to ensure immediate delivery.
        
        Args:
            response: JSON-RPC response dictionary
        """
        try:
            # Format as compact JSON (no extra whitespace)
            response_json = json.dumps(response, separators=(',', ':'))
            
            # Write as single line with newline
            self.output_stream.write(response_json)
            self.output_stream.write('\n')
            
            # Explicit flush to ensure immediate delivery
            self.output_stream.flush()
            
            logger.debug(f"Wrote response: {len(response_json)} bytes")
            
        except Exception as e:
            logger.error(f"Error writing to stdout: {e}")
            raise
    
    def write_error(self, error_code: int, message: str, request_id: Any = None) -> None:
        """
        Write JSON-RPC error response to stdout
        
        Args:
            error_code: JSON-RPC error code
            message: Error message
            request_id: Request ID (can be None for parse errors)
        """
        error_response = {
            'jsonrpc': '2.0',
            'error': {
                'code': error_code,
                'message': message
            },
            'id': request_id
        }
        
        self.write_response(error_response)
    
    def is_eof(self) -> bool:
        """
        Check if EOF has been reached
        
        Returns:
            True if EOF detected, False otherwise
        """
        return self._eof_reached


class InvalidRequestError(Exception):
    """Exception raised for invalid JSON-RPC requests"""
    pass


class MethodNotFoundError(Exception):
    """Exception raised for unknown methods"""
    pass


class RequestRouter:
    """Routes MCP requests to appropriate handlers with comprehensive error handling"""
    
    def __init__(self, socket_handler: SocketHandler, oauth_manager: OAuthManager):
        """
        Initialize request router
        
        Args:
            socket_handler: SocketHandler for communicating with ErlVectorDB
            oauth_manager: OAuthManager for authentication
        """
        self.socket_handler = socket_handler
        self.oauth_manager = oauth_manager
        self._request_counter = 1
        
        logger.debug("RequestRouter initialized")
    
    def handle_request(self, request: Dict) -> Dict:
        """
        Route request to appropriate handler with comprehensive error handling
        
        Args:
            request: JSON-RPC request dictionary
            
        Returns:
            JSON-RPC response dictionary
        """
        request_id = request.get('id')
        
        try:
            # Validate request structure
            self._validate_request(request)
            
            # Extract method and params
            method = request['method']
            params = request.get('params', {})
            
            logger.info(f"Routing request: method={method}, id={request_id}")
            
            # Dispatch to method-specific handler
            if method == 'initialize':
                return self.handle_initialize(params, request_id)
            elif method == 'tools/list':
                return self.handle_tools_list(params, request_id)
            elif method == 'tools/call':
                return self.handle_tools_call(params, request_id)
            else:
                raise MethodNotFoundError(f"Method not found: {method}")
        
        except json.JSONDecodeError as e:
            logger.error(f"JSON parse error: {e}")
            return self._create_error_response(-32700, f"Parse error: {str(e)}", request_id)
        
        except InvalidRequestError as e:
            logger.error(f"Invalid request: {e}")
            return self._create_error_response(-32600, f"Invalid request: {str(e)}", request_id)
        
        except MethodNotFoundError as e:
            logger.warning(f"Method not found: {e}")
            return self._create_error_response(-32601, str(e), request_id)
        
        except ConnectionError as e:
            logger.error(f"Connection error: {e}")
            return self._create_error_response(-32000, f"Connection error: {str(e)}", request_id)
        
        except AuthenticationError as e:
            logger.error(f"Authentication error: {e}")
            return self._create_error_response(-32000, f"Authentication error: {str(e)}", request_id)
        
        except Exception as e:
            logger.error(f"Internal error: {e}", exc_info=True)
            return self._create_error_response(-32603, f"Internal error: {str(e)}", request_id)
    
    def _validate_request(self, request: Dict) -> None:
        """
        Validate JSON-RPC request structure
        
        Args:
            request: Request dictionary to validate
            
        Raises:
            InvalidRequestError: If request is invalid
        """
        # Check required fields
        if not isinstance(request, dict):
            raise InvalidRequestError("Request must be a dictionary")
        
        if 'jsonrpc' not in request:
            raise InvalidRequestError("Missing 'jsonrpc' field")
        
        if request['jsonrpc'] != '2.0':
            raise InvalidRequestError(f"Invalid JSON-RPC version: {request['jsonrpc']}")
        
        if 'method' not in request:
            raise InvalidRequestError("Missing 'method' field")
        
        if not isinstance(request['method'], str):
            raise InvalidRequestError("'method' must be a string")
        
        # 'id' field is optional but if present should be string, number, or null
        if 'id' in request:
            request_id = request['id']
            if not isinstance(request_id, (str, int, float, type(None))):
                raise InvalidRequestError("'id' must be a string, number, or null")
        
        # 'params' field is optional but if present should be dict or list
        if 'params' in request:
            params = request['params']
            if not isinstance(params, (dict, list)):
                raise InvalidRequestError("'params' must be an object or array")
    
    def _create_error_response(self, code: int, message: str, request_id: Any) -> Dict:
        """
        Create JSON-RPC error response
        
        Args:
            code: Error code
            message: Error message
            request_id: Request ID (preserved from request)
            
        Returns:
            JSON-RPC error response dictionary
        """
        return {
            'jsonrpc': '2.0',
            'error': {
                'code': code,
                'message': message
            },
            'id': request_id
        }
    
    def _create_success_response(self, result: Any, request_id: Any) -> Dict:
        """
        Create JSON-RPC success response
        
        Args:
            result: Result data
            request_id: Request ID (preserved from request)
            
        Returns:
            JSON-RPC success response dictionary
        """
        return {
            'jsonrpc': '2.0',
            'result': result,
            'id': request_id
        }
    
    def handle_initialize(self, params: Dict, request_id: Any) -> Dict:
        """
        Handle initialize request
        
        Args:
            params: Request parameters
            request_id: Request ID
            
        Returns:
            JSON-RPC response dictionary
        """
        logger.info("Handling initialize request")
        
        try:
            # Ensure socket is connected
            if not self.socket_handler.is_connected():
                logger.info("Connecting to ErlVectorDB for initialize")
                self.socket_handler.connect()
                
                # Send initialize to ErlVectorDB
                init_request = {
                    'jsonrpc': '2.0',
                    'method': 'initialize',
                    'params': {
                        'protocolVersion': '2024-11-05',
                        'capabilities': {'tools': {}},
                        'clientInfo': {
                            'name': 'gemini-mcp-bridge',
                            'version': '1.0.0'
                        }
                    },
                    'id': self._get_next_request_id(),
                    'auth': self.oauth_manager.get_auth_dict()
                }
                
                self.socket_handler.send_message(init_request)
                init_response = self.socket_handler.receive_message()
                
                # Check for errors in ErlVectorDB initialize
                if 'error' in init_response:
                    logger.warning(f"ErlVectorDB initialize returned error: {init_response['error']}")
            
            # Return our server info
            result = {
                'protocolVersion': '2024-11-05',
                'capabilities': {
                    'tools': {}
                },
                'serverInfo': {
                    'name': 'erlvectordb',
                    'version': '0.1.0',
                    'description': 'ErlVectorDB Vector Database with OAuth 2.1'
                }
            }
            
            return self._create_success_response(result, request_id)
        
        except Exception as e:
            logger.error(f"Error in initialize handler: {e}")
            raise
    
    def handle_tools_list(self, params: Dict, request_id: Any) -> Dict:
        """
        Handle tools/list request
        
        Args:
            params: Request parameters
            request_id: Request ID
            
        Returns:
            JSON-RPC response dictionary
        """
        logger.info("Handling tools/list request")
        
        try:
            # Ensure connected
            if not self.socket_handler.is_connected():
                raise ConnectionError("Not connected to ErlVectorDB")
            
            # Forward to ErlVectorDB
            forward_request = {
                'jsonrpc': '2.0',
                'method': 'tools/list',
                'params': params,
                'id': self._get_next_request_id(),
                'auth': self.oauth_manager.get_auth_dict()
            }
            
            self.socket_handler.send_message(forward_request)
            response = self.socket_handler.receive_message()
            
            # Handle 401 Unauthorized
            if 'error' in response and response.get('error', {}).get('code') == 401:
                logger.warning("Received 401, refreshing token and retrying")
                self.oauth_manager.handle_401_response()
                
                # Retry with new token
                forward_request['auth'] = self.oauth_manager.get_auth_dict()
                self.socket_handler.send_message(forward_request)
                response = self.socket_handler.receive_message()
            
            # Return response with our request ID
            if 'result' in response:
                return self._create_success_response(response['result'], request_id)
            elif 'error' in response:
                return self._create_error_response(
                    response['error'].get('code', -32000),
                    response['error'].get('message', 'Unknown error'),
                    request_id
                )
            else:
                raise ValueError("Invalid response from ErlVectorDB")
        
        except Exception as e:
            logger.error(f"Error in tools/list handler: {e}")
            raise
    
    def handle_tools_call(self, params: Dict, request_id: Any) -> Dict:
        """
        Handle tools/call request
        
        Args:
            params: Request parameters
            request_id: Request ID
            
        Returns:
            JSON-RPC response dictionary
        """
        tool_name = params.get('name', 'unknown')
        logger.info(f"Handling tools/call request for tool: {tool_name}")
        
        try:
            # Ensure connected
            if not self.socket_handler.is_connected():
                raise ConnectionError("Not connected to ErlVectorDB")
            
            # Forward to ErlVectorDB
            forward_request = {
                'jsonrpc': '2.0',
                'method': 'tools/call',
                'params': params,
                'id': self._get_next_request_id(),
                'auth': self.oauth_manager.get_auth_dict()
            }
            
            self.socket_handler.send_message(forward_request)
            response = self.socket_handler.receive_message()
            
            # Handle 401 Unauthorized
            if 'error' in response and response.get('error', {}).get('code') == 401:
                logger.warning("Received 401, refreshing token and retrying")
                self.oauth_manager.handle_401_response()
                
                # Retry with new token
                forward_request['auth'] = self.oauth_manager.get_auth_dict()
                self.socket_handler.send_message(forward_request)
                response = self.socket_handler.receive_message()
            
            # Return response with our request ID
            if 'result' in response:
                return self._create_success_response(response['result'], request_id)
            elif 'error' in response:
                return self._create_error_response(
                    response['error'].get('code', -32000),
                    response['error'].get('message', 'Unknown error'),
                    request_id
                )
            else:
                raise ValueError("Invalid response from ErlVectorDB")
        
        except Exception as e:
            logger.error(f"Error in tools/call handler: {e}")
            raise
    
    def _get_next_request_id(self) -> int:
        """
        Get next request ID for forwarding to ErlVectorDB
        
        Returns:
            Next request ID
        """
        request_id = self._request_counter
        self._request_counter += 1
        return request_id


class MCPServer:
    """Main server orchestrator that coordinates all components"""
    
    def __init__(self, config: ServerConfig):
        """
        Initialize MCP server with all components
        
        Args:
            config: Server configuration
        """
        self.config = config
        self._shutdown_requested = False
        
        # Initialize components
        logger.info("Initializing MCP server components")
        
        # Create OAuth manager
        self.oauth_manager = OAuthManager(config)
        
        # Create socket handler (not connected yet)
        self.socket_handler = SocketHandler(
            host=config.erlvectordb_host,
            port=config.erlvectordb_port,
            timeout=config.socket_timeout,
            buffer_size=config.socket_buffer_size,
            max_reconnect_attempts=config.max_reconnect_attempts,
            reconnect_delay=config.reconnect_delay
        )
        
        # Create request router
        self.request_router = RequestRouter(self.socket_handler, self.oauth_manager)
        
        # Create stdio handler
        self.stdio_handler = StdioHandler()
        
        logger.info(f"MCP server initialized with config: {config}")
    
    def run(self) -> int:
        """
        Run the main server event loop
        
        Continuously reads requests from stdin, processes them, and writes
        responses to stdout until EOF or shutdown is requested.
        
        Returns:
            Exit code (0 for success, non-zero for error)
        """
        logger.info("Starting MCP server main event loop")
        logger.info("Waiting for requests on stdin...")
        
        # Set up signal handlers for graceful shutdown
        self._setup_signal_handlers()
        
        try:
            # Main event loop - continuously read from stdin
            # This implements the MCP stdio protocol where:
            # 1. Gemini CLI sends JSON-RPC requests via stdin (one per line)
            # 2. We process each request and send response via stdout
            # 3. Loop continues until stdin closes (EOF) or shutdown signal
            while not self._shutdown_requested:
                try:
                    # Read request from stdin
                    # This blocks until a line is available or EOF
                    request = self.stdio_handler.read_request()
                    
                    # Check for EOF
                    # None indicates stdin was closed by Gemini CLI
                    if request is None:
                        logger.info("EOF detected, initiating graceful shutdown")
                        break
                    
                    # Process request
                    # Router handles validation, dispatching, and error handling
                    response = self.request_router.handle_request(request)
                    
                    # Write response to stdout
                    # Explicit flush ensures immediate delivery to Gemini CLI
                    self.stdio_handler.write_response(response)
                    
                except json.JSONDecodeError as e:
                    # Parse error - send error response and continue
                    # This allows recovery from malformed requests
                    logger.error(f"JSON parse error: {e}")
                    self.stdio_handler.write_error(-32700, f"Parse error: {str(e)}", None)
                    
                except KeyboardInterrupt:
                    # User interrupted - graceful shutdown
                    logger.info("Keyboard interrupt received")
                    break
                    
                except Exception as e:
                    # Unexpected error - log and continue
                    # We try to keep the server running even after errors
                    logger.error(f"Unexpected error in main loop: {e}", exc_info=True)
                    # Try to send error response if we can
                    try:
                        self.stdio_handler.write_error(-32603, f"Internal error: {str(e)}", None)
                    except Exception:
                        pass
            
            # Normal shutdown
            logger.info("Main event loop exited normally")
            return 0
            
        except Exception as e:
            # Fatal error
            logger.error(f"Fatal error in main event loop: {e}", exc_info=True)
            return 1
            
        finally:
            # Cleanup
            self.shutdown()
    
    def shutdown(self) -> None:
        """
        Perform graceful shutdown
        
        Closes all connections and cleans up resources.
        """
        if self._shutdown_requested:
            return
        
        logger.info("Shutting down MCP server")
        self._shutdown_requested = True
        
        try:
            # Close socket connection
            if self.socket_handler:
                self.socket_handler.close()
                logger.info("Socket connection closed")
        except Exception as e:
            logger.warning(f"Error closing socket: {e}")
        
        logger.info("MCP server shutdown complete")
    
    def _setup_signal_handlers(self) -> None:
        """
        Set up signal handlers for graceful shutdown
        
        Handles SIGTERM and SIGINT signals.
        """
        import signal
        
        def signal_handler(signum, frame):
            """Handle shutdown signals"""
            signal_name = signal.Signals(signum).name
            logger.info(f"Received signal {signal_name}, initiating graceful shutdown")
            self._shutdown_requested = True
        
        # Register signal handlers
        signal.signal(signal.SIGTERM, signal_handler)
        signal.signal(signal.SIGINT, signal_handler)
        
        logger.debug("Signal handlers registered for SIGTERM and SIGINT")


# Keep old class for backward compatibility (deprecated)
class ErlVectorDBMCPServer:
    """
    DEPRECATED: Use MCPServer instead.
    
    MCP Server that bridges Gemini CLI to ErlVectorDB.
    This class is kept for backward compatibility but should not be used in new code.
    """
    
    def __init__(self, config: ServerConfig):
        """Initialize server with configuration"""
        logger.warning("ErlVectorDBMCPServer is deprecated, use MCPServer instead")
        self.config = config
        
        # State
        self.oauth_manager = OAuthManager(config)
        self.socket_handler = None
        self.request_id = 1
        
        logger.info(f"Initialized ErlVectorDB MCP Server")
        logger.info(f"Configuration: {self.config}")
    
    def get_access_token(self) -> str:
        """Get OAuth access token from ErlVectorDB"""
        try:
            return self.oauth_manager.get_token()
        except AuthenticationError as e:
            logger.error(f"Failed to get OAuth token: {e}")
            raise
    
    def connect_to_erlvectordb(self):
        """Connect to ErlVectorDB MCP server"""
        if self.socket_handler and self.socket_handler.is_connected():
            return
        
        try:
            # Get OAuth token first
            self.get_access_token()
            
            # Create socket handler and connect
            logger.info(f"Connecting to ErlVectorDB at {self.config.erlvectordb_host}:{self.config.erlvectordb_port}")
            self.socket_handler = SocketHandler(
                host=self.config.erlvectordb_host,
                port=self.config.erlvectordb_port,
                timeout=self.config.socket_timeout,
                buffer_size=self.config.socket_buffer_size,
                max_reconnect_attempts=self.config.max_reconnect_attempts,
                reconnect_delay=self.config.reconnect_delay
            )
            self.socket_handler.connect()
            
            # Initialize MCP connection
            init_response = self.send_to_erlvectordb('initialize', {
                'protocolVersion': '2024-11-05',
                'capabilities': {'tools': {}},
                'clientInfo': {
                    'name': 'gemini-mcp-bridge',
                    'version': '1.0.0'
                }
            })
            
            logger.info("Connected to ErlVectorDB successfully")
        except Exception as e:
            logger.error(f"Failed to connect to ErlVectorDB: {e}")
            if self.socket_handler:
                self.socket_handler.close()
            raise
    
    def send_to_erlvectordb(self, method: str, params: Dict = None) -> Dict:
        """Send request to ErlVectorDB MCP server"""
        if not self.socket_handler or not self.socket_handler.is_connected():
            self.connect_to_erlvectordb()
        
        request_id = self.request_id
        self.request_id += 1
        
        # Get auth dict from OAuth manager
        auth_dict = self.oauth_manager.get_auth_dict()
        
        request = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params or {},
            'id': request_id,
            'auth': auth_dict
        }
        
        try:
            # Send request using socket handler
            self.socket_handler.send_message(request)
            
            # Receive response using socket handler
            response = self.socket_handler.receive_message()
            
            # Check for 401 Unauthorized
            if 'error' in response and response['error'].get('code') == 401:
                logger.warning("Received 401 Unauthorized, refreshing token and retrying")
                
                # Refresh token
                self.oauth_manager.handle_401_response()
                
                # Retry request with new token
                auth_dict = self.oauth_manager.get_auth_dict()
                request['auth'] = auth_dict
                
                self.socket_handler.send_message(request)
                response = self.socket_handler.receive_message()
            
            return response
        except Exception as e:
            logger.error(f"Error communicating with ErlVectorDB: {e}")
            if self.socket_handler:
                self.socket_handler.close()
            raise
    
    def handle_initialize(self, params: Dict, request_id: Any) -> Dict:
        """Handle MCP initialize request"""
        logger.info("Handling initialize request")
        
        # Connect to ErlVectorDB
        try:
            self.connect_to_erlvectordb()
        except Exception as e:
            return {
                'jsonrpc': '2.0',
                'error': {
                    'code': -32000,
                    'message': f'Failed to connect to ErlVectorDB: {str(e)}'
                },
                'id': request_id
            }
        
        # Return our server info
        return {
            'jsonrpc': '2.0',
            'result': {
                'protocolVersion': '2024-11-05',
                'capabilities': {
                    'tools': {}
                },
                'serverInfo': {
                    'name': 'erlvectordb',
                    'version': '0.1.0',
                    'description': 'ErlVectorDB Vector Database with OAuth 2.1'
                }
            },
            'id': request_id
        }
    
    def handle_tools_list(self, params: Dict, request_id: Any) -> Dict:
        """Handle tools/list request"""
        logger.info("Handling tools/list request")
        
        try:
            # Forward to ErlVectorDB
            response = self.send_to_erlvectordb('tools/list', params)
            
            # Return the response with our request ID
            if 'result' in response:
                return {
                    'jsonrpc': '2.0',
                    'result': response['result'],
                    'id': request_id
                }
            elif 'error' in response:
                return {
                    'jsonrpc': '2.0',
                    'error': response['error'],
                    'id': request_id
                }
            else:
                return {
                    'jsonrpc': '2.0',
                    'error': {
                        'code': -32603,
                        'message': 'Invalid response from ErlVectorDB'
                    },
                    'id': request_id
                }
        except Exception as e:
            logger.error(f"Error in tools/list: {e}")
            return {
                'jsonrpc': '2.0',
                'error': {
                    'code': -32000,
                    'message': str(e)
                },
                'id': request_id
            }
    
    def handle_tools_call(self, params: Dict, request_id: Any) -> Dict:
        """Handle tools/call request"""
        tool_name = params.get('name', 'unknown')
        logger.info(f"Handling tools/call request for tool: {tool_name}")
        
        try:
            # Forward to ErlVectorDB
            response = self.send_to_erlvectordb('tools/call', params)
            
            # Return the response with our request ID
            if 'result' in response:
                return {
                    'jsonrpc': '2.0',
                    'result': response['result'],
                    'id': request_id
                }
            elif 'error' in response:
                return {
                    'jsonrpc': '2.0',
                    'error': response['error'],
                    'id': request_id
                }
            else:
                return {
                    'jsonrpc': '2.0',
                    'error': {
                        'code': -32603,
                        'message': 'Invalid response from ErlVectorDB'
                    },
                    'id': request_id
                }
        except Exception as e:
            logger.error(f"Error in tools/call: {e}")
            return {
                'jsonrpc': '2.0',
                'error': {
                    'code': -32000,
                    'message': str(e)
                },
                'id': request_id
            }
    
    def handle_request(self, request: Dict) -> Dict:
        """Handle incoming MCP request"""
        method = request.get('method')
        params = request.get('params', {})
        request_id = request.get('id')
        
        logger.info(f"Received request: method={method}, id={request_id}")
        
        if method == 'initialize':
            return self.handle_initialize(params, request_id)
        elif method == 'tools/list':
            return self.handle_tools_list(params, request_id)
        elif method == 'tools/call':
            return self.handle_tools_call(params, request_id)
        else:
            logger.warning(f"Unknown method: {method}")
            return {
                'jsonrpc': '2.0',
                'error': {
                    'code': -32601,
                    'message': f'Method not found: {method}'
                },
                'id': request_id
            }
    
    def run(self):
        """Run the MCP server (stdio mode)"""
        logger.info("Starting ErlVectorDB MCP Server in stdio mode")
        logger.info("Waiting for requests on stdin...")
        
        try:
            # Read from stdin, write to stdout
            for line in sys.stdin:
                line = line.strip()
                if not line:
                    continue
                
                try:
                    # Parse request
                    request = json.loads(line)
                    logger.debug(f"Parsed request: {request}")
                    
                    # Handle request
                    response = self.handle_request(request)
                    
                    # Send response to stdout
                    response_json = json.dumps(response)
                    print(response_json, flush=True)
                    logger.debug(f"Sent response: {response_json}")
                    
                except json.JSONDecodeError as e:
                    logger.error(f"Invalid JSON: {e}")
                    error_response = {
                        'jsonrpc': '2.0',
                        'error': {
                            'code': -32700,
                            'message': 'Parse error'
                        },
                        'id': None
                    }
                    print(json.dumps(error_response), flush=True)
                except Exception as e:
                    logger.error(f"Error handling request: {e}", exc_info=True)
                    error_response = {
                        'jsonrpc': '2.0',
                        'error': {
                            'code': -32603,
                            'message': f'Internal error: {str(e)}'
                        },
                        'id': None
                    }
                    print(json.dumps(error_response), flush=True)
        
        except KeyboardInterrupt:
            logger.info("Shutting down (interrupted)")
        except Exception as e:
            logger.error(f"Fatal error: {e}", exc_info=True)
        finally:
            if self.socket_handler:
                self.socket_handler.close()
            logger.info("Server stopped")

def main():
    """Main entry point"""
    try:
        # Load configuration from environment
        config = ServerConfig.from_environment()
        
        # Validate configuration
        config.validate()
        
        # Configure logging level
        logger.setLevel(config.log_level.upper())
        
        logger.info(f"ErlVectorDB MCP Server for Gemini CLI")
        logger.info(f"Target: {config.erlvectordb_host}:{config.erlvectordb_port}")
        logger.info(f"OAuth: {config.oauth_host}:{config.oauth_port}")
        
        # Create and run server
        server = MCPServer(config)
        exit_code = server.run()
        sys.exit(exit_code)
        
    except ValueError as e:
        logger.error(f"Configuration error: {e}")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Fatal error: {e}", exc_info=True)
        sys.exit(1)

if __name__ == '__main__':
    main()
