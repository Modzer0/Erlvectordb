# Implementation Plan: Port Management System

## Overview

This implementation plan creates a robust port management system for ErlVectorDB that handles port conflicts, provides flexible configuration, and ensures reliable service startup. The implementation follows a modular approach with clear separation of concerns.

## Tasks

- [x] 1. Create core port management infrastructure
  - Create port_manager gen_server module
  - Implement port_registry for tracking allocations
  - Set up basic configuration loading
  - _Requirements: 1.1, 2.1, 3.4_

- [x] 1.1 Write property test for port conflict detection
  - **Property 1: Port Conflict Detection**
  - **Validates: Requirements 1.1**

- [x] 2. Implement port allocation and conflict resolution
  - [x] 2.1 Create port binding logic with conflict detection
    - Implement gen_tcp:listen with conflict detection
    - Add automatic fallback to next available port
    - _Requirements: 1.1, 1.2_

  - [x] 2.2 Write property test for automatic fallback selection
    - **Property 2: Automatic Fallback Selection**
    - **Validates: Requirements 1.2**

  - [x] 2.3 Add port range validation and exhaustion handling
    - Validate port ranges (1024-65535)
    - Handle range exhaustion with descriptive errors
    - _Requirements: 1.3, 1.5_

  - [x] 2.4 Write property test for range validation
    - **Property 5: Port Range Validation**
    - **Validates: Requirements 1.5**

- [ ] 3. Implement configuration management
  - [ ] 3.1 Create port_config module for configuration loading
    - Support application environment variables
    - Support configuration files
    - Implement configuration validation
    - _Requirements: 2.1, 2.5_

  - [ ] 3.2 Write property test for configuration loading
    - **Property 6: Configuration Loading**
    - **Validates: Requirements 2.1**

  - [ ] 3.3 Add per-service configuration support
    - Separate configuration for MCP, OAuth, REST API
    - Custom port ranges per service
    - Default configuration fallback
    - _Requirements: 2.2, 2.3, 2.4_

  - [ ] 3.4 Write property test for per-service configuration
    - **Property 9: Per-Service Configuration**
    - **Validates: Requirements 2.4**

- [ ] 4. Checkpoint - Ensure basic port management works
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 5. Implement service startup coordination
  - [ ] 5.1 Create startup sequencing logic
    - Pre-allocate all ports before starting services
    - Implement dependency-ordered service startup
    - Add startup failure cleanup
    - _Requirements: 4.1, 4.2, 4.3_

  - [ ] 5.2 Write property test for startup sequencing
    - **Property 16: Pre-Service Port Binding**
    - **Validates: Requirements 4.1**

  - [ ] 5.3 Add comprehensive error handling and reporting
    - Clear error messages for startup failures
    - Resource cleanup on partial failures
    - _Requirements: 4.4, 4.5_

  - [ ] 5.4 Write property test for startup cleanup
    - **Property 17: Startup Failure Cleanup**
    - **Validates: Requirements 4.2**

- [ ] 6. Implement port status and monitoring
  - [ ] 6.1 Create port status API endpoints
    - Add REST endpoint for port status queries
    - Implement port registry status reporting
    - _Requirements: 3.1, 3.2, 3.3_

  - [ ] 6.2 Write property test for port status API
    - **Property 12: Port Status API Availability**
    - **Validates: Requirements 3.2**

  - [ ] 6.3 Add port registry maintenance
    - Track port allocations and releases
    - Update registry on service stop
    - _Requirements: 3.4, 3.5_

  - [ ] 6.4 Write property test for registry maintenance
    - **Property 14: Port Registry Maintenance**
    - **Validates: Requirements 3.4**

- [ ] 7. Implement development mode features
  - [ ] 7.1 Add development mode port selection
    - Automatic port selection from base ports
    - Shorter timeouts for development
    - _Requirements: 5.1, 5.3_

  - [ ] 7.2 Write property test for development mode
    - **Property 21: Development Mode Port Selection**
    - **Validates: Requirements 5.1**

  - [ ] 7.3 Create force restart functionality
    - Command-line option to kill existing instances
    - Force restart with proper cleanup sequencing
    - _Requirements: 5.2, 5.4, 5.5_

- [ ] 8. Implement container deployment support
  - [ ] 8.1 Add container-specific configuration
    - Support binding to all interfaces (0.0.0.0)
    - Respect PORT environment variables
    - _Requirements: 6.1, 6.2_

  - [ ] 8.2 Write property test for container configuration
    - **Property 24: Container Interface Binding**
    - **Validates: Requirements 6.1**

  - [ ] 8.3 Add health check endpoints and graceful shutdown
    - Health check endpoints for orchestration
    - SIGTERM signal handling for graceful shutdown
    - Container port mapping logging
    - _Requirements: 6.3, 6.4, 6.5_

  - [ ] 8.4 Write property test for graceful shutdown
    - **Property 28: Graceful Shutdown Signal Handling**
    - **Validates: Requirements 6.5**

- [ ] 9. Integration and service updates
  - [ ] 9.1 Update existing services to use port manager
    - Modify mcp_server to use port_manager
    - Update oauth_server for port management integration
    - Update rest_api_server for port management integration
    - _Requirements: 1.1, 1.2, 3.1_

  - [ ] 9.2 Write integration tests for service coordination
    - Test actual service startup with port management
    - Test port conflict resolution with real services
    - _Requirements: 4.1, 4.2, 4.3_

  - [ ] 9.3 Update application supervisor for port management
    - Integrate port_manager into supervision tree
    - Update startup sequence in erlvectordb_sup
    - _Requirements: 4.1, 4.3_

- [ ] 10. Final checkpoint and documentation
  - [ ] 10.1 Create configuration examples and documentation
    - Update sys.config with port management options
    - Create development configuration examples
    - Document container deployment configuration
    - _Requirements: 2.1, 5.1, 6.1_

  - [ ] 10.2 Final integration testing
    - Test complete application startup with port management
    - Verify all services work with new port management
    - Test configuration loading from all sources
    - _Requirements: All requirements_

- [ ] 11. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties
- Integration tests validate end-to-end functionality
- The implementation prioritizes backward compatibility with existing configuration