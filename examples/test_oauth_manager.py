#!/usr/bin/env python3

"""
Tests for OAuthManager class

This module contains both property-based tests and unit tests for the OAuthManager
class that manages OAuth token acquisition, caching, and refresh.
"""

import json
import time
from typing import Dict, Any
from unittest.mock import Mock, patch, MagicMock
from hypothesis import given, strategies as st, settings, assume
import pytest
import requests

# Import the OAuthManager class
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))
from gemini_mcp_server import OAuthManager, ServerConfig, AuthenticationError


# ============================================================================
# Property-Based Tests
# ============================================================================

# Feature: gemini-mcp-server-fix, Property 14: OAuth retry with backoff
@given(
    num_failures=st.integers(min_value=1, max_value=5),
    initial_backoff=st.floats(min_value=0.1, max_value=2.0),
    multiplier=st.floats(min_value=1.5, max_value=3.0),
    max_backoff=st.floats(min_value=5.0, max_value=60.0)
)
@settings(max_examples=20, deadline=None)  # Disable deadline since we're testing actual delays
def test_oauth_retry_with_backoff_property(num_failures, initial_backoff, multiplier, max_backoff):
    """
    Property 14: OAuth retry with backoff
    
    For any OAuth token request failure, the server should retry with 
    exponentially increasing delays.
    
    Validates: Requirements 8.2
    """
    # Ensure max_backoff is greater than initial_backoff
    assume(max_backoff > initial_backoff)
    
    # Create config with test parameters
    config = ServerConfig(
        oauth_max_retries=num_failures + 1,  # Will succeed on last attempt
        oauth_initial_backoff=initial_backoff,
        oauth_backoff_multiplier=multiplier,
        oauth_max_backoff=max_backoff
    )
    
    manager = OAuthManager(config)
    
    # Track retry attempts and sleep calls
    attempt_count = [0]
    sleep_calls = []
    
    def mock_request_token():
        """Mock that fails num_failures times, then succeeds"""
        attempt_count[0] += 1
        
        if attempt_count[0] <= num_failures:
            # Fail
            raise requests.exceptions.ConnectionError("Mock connection error")
        else:
            # Succeed on last attempt
            return {
                'access_token': 'test_token_success',
                'token_type': 'Bearer',
                'expires_in': 3600
            }
    
    def mock_sleep(duration):
        """Mock sleep to track delays without actually sleeping"""
        sleep_calls.append(duration)
    
    # Patch both _request_token and time.sleep
    with patch.object(manager, '_request_token', side_effect=mock_request_token), \
         patch('time.sleep', side_effect=mock_sleep):
        # Should succeed after retries
        token = manager.refresh_token()
        
        # Verify token was obtained
        assert token == 'test_token_success'
        
        # Verify correct number of attempts
        assert attempt_count[0] == num_failures + 1
        
        # Verify exponential backoff delays
        # Should have num_failures sleep calls (one after each failure)
        assert len(sleep_calls) == num_failures
        
        for i, delay in enumerate(sleep_calls):
            # Calculate expected backoff for this attempt
            expected_backoff = initial_backoff * (multiplier ** i)
            expected_backoff = min(expected_backoff, max_backoff)
            
            # Verify the delay matches expected backoff
            assert abs(delay - expected_backoff) < 0.01, \
                f"Delay {delay:.2f}s does not match expected {expected_backoff:.2f}s"


# Feature: gemini-mcp-server-fix, Property 8: Token refresh on 401
@given(
    initial_token=st.text(min_size=10, max_size=50),
    refreshed_token=st.text(min_size=10, max_size=50)
)
@settings(max_examples=20, deadline=3000)
def test_token_refresh_on_401_property(initial_token, refreshed_token):
    """
    Property 8: Token refresh on 401
    
    For any 401 Unauthorized response from ErlVectorDB, the server should 
    obtain a new OAuth token and retry the request.
    
    Validates: Requirements 3.4, 8.3
    """
    # Ensure tokens are different
    assume(initial_token != refreshed_token)
    
    config = ServerConfig()
    manager = OAuthManager(config)
    
    # Set initial token
    manager._access_token = initial_token
    manager._token_expiry = time.time() + 3600
    
    # Mock the token request to return new token
    def mock_request_token():
        return {
            'access_token': refreshed_token,
            'token_type': 'Bearer',
            'expires_in': 3600
        }
    
    with patch.object(manager, '_request_token', side_effect=mock_request_token):
        # Simulate 401 response
        new_token = manager.handle_401_response()
        
        # Verify new token was obtained
        assert new_token == refreshed_token
        assert new_token != initial_token
        
        # Verify token was cached
        assert manager._access_token == refreshed_token
        
        # Verify old token was invalidated before refresh
        # (the handle_401_response should have cleared it)
        assert manager.is_token_valid()


# ============================================================================
# Unit Tests
# ============================================================================

class TestOAuthManagerTokenAcquisition:
    """Unit tests for OAuth token acquisition"""
    
    def test_token_acquisition_success(self):
        """Test successful token acquisition"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Mock successful token request
        mock_response = {
            'access_token': 'test_access_token_123',
            'token_type': 'Bearer',
            'expires_in': 3600
        }
        
        with patch.object(manager, '_request_token', return_value=mock_response):
            token = manager.get_token()
            
            assert token == 'test_access_token_123'
            assert manager._access_token == 'test_access_token_123'
            assert manager._token_type == 'Bearer'
            assert manager.is_token_valid()
    
    def test_token_acquisition_failure_with_retry(self):
        """Test token acquisition failure with retry logic"""
        config = ServerConfig(
            oauth_max_retries=3,
            oauth_initial_backoff=0.1,
            oauth_backoff_multiplier=2.0
        )
        manager = OAuthManager(config)
        
        # Mock that always fails
        with patch.object(manager, '_request_token', side_effect=requests.exceptions.ConnectionError("Connection failed")):
            with pytest.raises(AuthenticationError) as exc_info:
                manager.get_token()
            
            assert 'Failed to acquire OAuth token after 3 attempts' in str(exc_info.value)
    
    def test_token_refresh_on_expiration(self):
        """Test automatic token refresh when token expires"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Set an expired token
        manager._access_token = 'old_token'
        manager._token_expiry = time.time() - 100  # Expired 100 seconds ago
        
        # Mock new token request
        mock_response = {
            'access_token': 'new_token',
            'token_type': 'Bearer',
            'expires_in': 3600
        }
        
        with patch.object(manager, '_request_token', return_value=mock_response):
            token = manager.get_token()
            
            # Should get new token
            assert token == 'new_token'
            assert manager._access_token == 'new_token'
    
    def test_token_refresh_on_401_response(self):
        """Test token refresh when receiving 401 response"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Set initial token
        manager._access_token = 'old_token'
        manager._token_expiry = time.time() + 3600
        
        # Mock new token request
        mock_response = {
            'access_token': 'refreshed_token',
            'token_type': 'Bearer',
            'expires_in': 3600
        }
        
        with patch.object(manager, '_request_token', return_value=mock_response):
            new_token = manager.handle_401_response()
            
            # Should get new token
            assert new_token == 'refreshed_token'
            assert manager._access_token == 'refreshed_token'
            assert new_token != 'old_token'


class TestOAuthManagerCaching:
    """Unit tests for OAuth token caching"""
    
    def test_cached_token_reuse(self):
        """Test that valid cached tokens are reused"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Set a valid token
        manager._access_token = 'cached_token'
        manager._token_expiry = time.time() + 3600
        
        # Get token should return cached token without making request
        with patch.object(manager, '_request_token') as mock_request:
            token = manager.get_token()
            
            assert token == 'cached_token'
            mock_request.assert_not_called()
    
    def test_token_expiry_tracking(self):
        """Test that token expiry is tracked correctly"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Mock token request
        mock_response = {
            'access_token': 'test_token',
            'token_type': 'Bearer',
            'expires_in': 3600
        }
        
        with patch.object(manager, '_request_token', return_value=mock_response):
            before_time = time.time()
            manager.refresh_token()
            after_time = time.time()
            
            # Expiry should be set to ~3600 seconds from now (minus 60 second buffer)
            expected_expiry = before_time + 3600 - 60
            assert manager._token_expiry >= expected_expiry - 1
            assert manager._token_expiry <= after_time + 3600 - 60 + 1
    
    def test_is_token_valid(self):
        """Test token validity checking"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # No token
        assert not manager.is_token_valid()
        
        # Valid token
        manager._access_token = 'valid_token'
        manager._token_expiry = time.time() + 3600
        assert manager.is_token_valid()
        
        # Expired token
        manager._token_expiry = time.time() - 100
        assert not manager.is_token_valid()
        
        # Token without expiry
        manager._access_token = 'token'
        manager._token_expiry = None
        assert not manager.is_token_valid()


class TestOAuthManagerHelpers:
    """Unit tests for OAuth manager helper methods"""
    
    def test_get_auth_header(self):
        """Test getting authorization header"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Set token
        manager._access_token = 'test_token_123'
        manager._token_expiry = time.time() + 3600
        manager._token_type = 'Bearer'
        
        header = manager.get_auth_header()
        
        assert header == {'Authorization': 'Bearer test_token_123'}
    
    def test_get_auth_dict(self):
        """Test getting auth dictionary for MCP requests"""
        config = ServerConfig()
        manager = OAuthManager(config)
        
        # Set token
        manager._access_token = 'test_token_456'
        manager._token_expiry = time.time() + 3600
        
        auth_dict = manager.get_auth_dict()
        
        assert auth_dict == {
            'type': 'bearer',
            'token': 'test_token_456'
        }


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
