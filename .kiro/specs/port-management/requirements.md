# Requirements Document

## Introduction

The ErlVectorDB application needs robust port management to handle port conflicts gracefully and provide flexible configuration options for different deployment scenarios. Currently, the application fails to start when default ports are already in use, which creates a poor user experience.

## Glossary

- **Port_Manager**: The system component responsible for managing network port allocation and conflict resolution
- **Service_Port**: A network port used by a specific service (MCP, OAuth, REST API)
- **Port_Conflict**: A situation where a requested port is already in use by another process
- **Fallback_Port**: An alternative port used when the primary port is unavailable
- **Dynamic_Port**: A port automatically selected by the system from available ports

## Requirements

### Requirement 1: Port Conflict Detection and Resolution

**User Story:** As a system administrator, I want the application to detect port conflicts and automatically resolve them, so that the application can start successfully even when default ports are occupied.

#### Acceptance Criteria

1. WHEN the application attempts to bind to a port that is already in use, THE Port_Manager SHALL detect the conflict and log a warning message
2. WHEN a port conflict is detected, THE Port_Manager SHALL attempt to bind to the next available port in a predefined range
3. WHEN no ports are available in the predefined range, THE Port_Manager SHALL return a descriptive error message
4. WHEN a fallback port is used, THE Port_Manager SHALL log the actual port being used for each service
5. THE Port_Manager SHALL validate that all selected ports are within valid ranges (1024-65535 for non-privileged ports)

### Requirement 2: Configurable Port Ranges

**User Story:** As a developer, I want to configure custom port ranges for different services, so that I can deploy the application in environments with specific networking requirements.

#### Acceptance Criteria

1. THE System SHALL read port configuration from application environment variables or configuration files
2. WHEN custom port ranges are specified, THE Port_Manager SHALL only select ports within those ranges
3. WHEN no custom configuration is provided, THE System SHALL use sensible default port ranges
4. THE System SHALL support separate port range configuration for MCP server, OAuth server, and REST API server
5. WHEN invalid port ranges are configured, THE System SHALL return a configuration error with specific details

### Requirement 3: Port Status Reporting

**User Story:** As a system operator, I want to see which ports are being used by each service, so that I can configure firewalls and load balancers correctly.

#### Acceptance Criteria

1. WHEN the application starts successfully, THE System SHALL log the actual ports used by each service
2. THE System SHALL provide an API endpoint to query current port assignments
3. WHEN queried for port status, THE System SHALL return service names, assigned ports, and binding status
4. THE Port_Manager SHALL maintain a registry of all active port bindings
5. WHEN a service stops, THE Port_Manager SHALL update the port registry to reflect the released port

### Requirement 4: Graceful Startup Sequence

**User Story:** As a user, I want the application to start services in the correct order with proper error handling, so that partial failures don't leave the system in an inconsistent state.

#### Acceptance Criteria

1. THE System SHALL attempt to bind all required ports before starting any services
2. WHEN port binding fails for any service, THE System SHALL release all previously bound ports and terminate gracefully
3. WHEN all ports are successfully bound, THE System SHALL start services in dependency order
4. THE System SHALL provide clear error messages indicating which service failed to start and why
5. WHEN startup fails, THE System SHALL clean up any partially initialized resources

### Requirement 5: Development Mode Support

**User Story:** As a developer, I want to easily restart the application during development without port conflicts, so that I can iterate quickly on code changes.

#### Acceptance Criteria

1. WHEN running in development mode, THE System SHALL support automatic port selection starting from configurable base ports
2. THE System SHALL provide a command-line option to kill existing instances before starting
3. WHEN development mode is enabled, THE System SHALL use shorter timeouts for port binding attempts
4. THE System SHALL support a "force restart" option that terminates existing processes on the same ports
5. WHEN force restart is used, THE System SHALL wait for proper cleanup before attempting to bind ports

### Requirement 6: Docker and Container Support

**User Story:** As a DevOps engineer, I want to deploy the application in containers with predictable port behavior, so that container orchestration works reliably.

#### Acceptance Criteria

1. THE System SHALL support binding to all interfaces (0.0.0.0) when configured for container deployment
2. WHEN running in a container, THE System SHALL respect PORT environment variables for each service
3. THE System SHALL provide health check endpoints on known ports for container orchestration
4. WHEN container port mapping is used, THE System SHALL log both internal and external port mappings
5. THE System SHALL support graceful shutdown signals (SIGTERM) for proper container lifecycle management