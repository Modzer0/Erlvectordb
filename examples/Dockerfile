# Multi-stage Dockerfile for ErlVectorDB with port management
FROM erlang:25-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    curl \
    build-base \
    openssl-dev

# Set working directory
WORKDIR /app

# Copy rebar configuration
COPY rebar.config rebar.lock ./

# Copy source code
COPY src/ src/
COPY include/ include/
COPY priv/ priv/
COPY test/ test/

# Build the application
RUN rebar3 compile
RUN rebar3 release

# Production stage
FROM erlang:25-alpine AS runtime

# Install runtime dependencies
RUN apk add --no-cache \
    openssl \
    curl \
    bash

# Create app user
RUN addgroup -g 1001 -S erlvectordb && \
    adduser -u 1001 -S erlvectordb -G erlvectordb

# Set working directory
WORKDIR /app

# Copy release from builder stage
COPY --from=builder /app/_build/default/rel/erlvectordb ./
COPY --chown=erlvectordb:erlvectordb --from=builder /app/_build/default/rel/erlvectordb ./

# Copy configuration files
COPY config/ config/
COPY examples/container-entrypoint.sh ./entrypoint.sh

# Make entrypoint executable
RUN chmod +x ./entrypoint.sh

# Create data and backup directories
RUN mkdir -p /data /backups && \
    chown -R erlvectordb:erlvectordb /data /backups /app

# Set container environment variables
ENV CONTAINER=true
ENV BIND_ALL_INTERFACES=true
ENV LOG_PORT_MAPPINGS=true
ENV GRACEFUL_SHUTDOWN_TIMEOUT=30

# Expose ports
EXPOSE 8080 8081 8082

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Switch to app user
USER erlvectordb

# Set entrypoint
ENTRYPOINT ["./entrypoint.sh"]
CMD ["foreground"]