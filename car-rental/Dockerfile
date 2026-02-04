# Multi-stage build for smaller final image
FROM haskell:8.10 as build

WORKDIR /build

# Copy configuration files
COPY car-rental.cabal stack.yaml ./

# Install dependencies only (for better caching)
RUN stack setup
RUN stack build --only-dependencies

# Copy source code and build
COPY . .
RUN stack build

# Extract the built executable
RUN cp $(stack path --local-install-root)/bin/car-rental /build/

# Final stage
FROM debian:buster-slim

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the executable from the build stage
COPY --from=build /build/car-rental /app/
COPY public /app/public

# Set executable permissions
RUN chmod +x /app/car-rental

# Run the application
EXPOSE 3000
CMD ["/app/car-rental"]