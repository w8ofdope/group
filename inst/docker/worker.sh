#!/bin/bash
# Internet Structure Data Collection Worker Launcher
# This script manages the lifecycle of the R worker process

set -e  # Exit on any error

# Configuration
WORKER_SCRIPT="/app/scripts/worker.R"
LOG_DIR="/app/logs"
PID_FILE="/app/worker.pid"
HEALTH_CHECK_INTERVAL=60

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[$(date '+%Y-%m-%d %H:%M:%S')] INFO: $1${NC}" >&2
}

log_warn() {
    echo -e "${YELLOW}[$(date '+%Y-%m-%d %H:%M:%S')] WARN: $1${NC}" >&2
}

log_error() {
    echo -e "${RED}[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $1${NC}" >&2
}

# Cleanup function
cleanup() {
    log_info "Cleaning up..."

    # Remove PID file if it exists
    if [ -f "$PID_FILE" ]; then
        rm -f "$PID_FILE"
        log_info "Removed PID file"
    fi

    # Kill any remaining worker processes
    if pgrep -f "worker.R" > /dev/null; then
        log_warn "Terminating remaining worker processes..."
        pkill -f "worker.R" || true
    fi

    exit 0
}

# Signal handling
trap cleanup SIGTERM SIGINT

# Health check function
health_check() {
    # Check if worker process is still running
    if [ -f "$PID_FILE" ]; then
        WORKER_PID=$(cat "$PID_FILE")
        if ! kill -0 "$WORKER_PID" 2>/dev/null; then
            log_error "Worker process $WORKER_PID is not running"
            return 1
        fi
    else
        log_error "PID file does not exist"
        return 1
    fi

    # Check database connectivity
    if ! pg_isready -h "${DB_HOST:-localhost}" -U "${DB_USER:-internet_user}" -d "${DB_NAME:-internet_structure}" >/dev/null 2>&1; then
        log_error "Database is not accessible"
        return 1
    fi

    # Check Redis connectivity (if configured)
    if [ -n "${REDIS_HOST:-}" ]; then
        if ! redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT:-6379}" ping >/dev/null 2>&1; then
            log_warn "Redis is not accessible"
            # Don't fail health check for Redis issues
        fi
    fi

    return 0
}

# Function to wait for database
wait_for_database() {
    local max_attempts=30
    local attempt=1

    log_info "Waiting for database to be ready..."

    while [ $attempt -le $max_attempts ]; do
        if pg_isready -h "${DB_HOST:-localhost}" -U "${DB_USER:-internet_user}" -d "${DB_NAME:-internet_structure}" >/dev/null 2>&1; then
            log_info "Database is ready"
            return 0
        fi

        log_info "Database not ready yet, attempt $attempt/$max_attempts"
        sleep 10
        attempt=$((attempt + 1))
    done

    log_error "Database did not become ready within timeout"
    return 1
}

# Function to start worker
start_worker() {
    log_info "Starting Internet Structure Data Collection Worker..."

    # Create log directory if it doesn't exist
    mkdir -p "$LOG_DIR"

    # Wait for database to be ready
    if ! wait_for_database; then
        log_error "Cannot start worker without database connection"
        exit 1
    fi

    # Check if worker is already running
    if [ -f "$PID_FILE" ]; then
        WORKER_PID=$(cat "$PID_FILE")
        if kill -0 "$WORKER_PID" 2>/dev/null; then
            log_warn "Worker is already running with PID $WORKER_PID"
            exit 1
        else
            log_warn "Removing stale PID file"
            rm -f "$PID_FILE"
        fi
    fi

    # Start the worker process
    log_info "Launching R worker script..."

    # Export environment variables for the worker
    export R_LIBS_USER="/usr/local/lib/R/site-library"
    export R_ENV="${R_ENV:-production}"

    # Start worker in background
    Rscript "$WORKER_SCRIPT" &
    WORKER_PID=$!

    # Store PID
    echo $WORKER_PID > "$PID_FILE"
    log_info "Worker started with PID $WORKER_PID"

    # Wait a moment for worker to initialize
    sleep 5

    # Initial health check
    if ! health_check; then
        log_error "Worker failed initial health check"
        cleanup
        exit 1
    fi

    log_info "Worker started successfully"
}

# Function to stop worker
stop_worker() {
    log_info "Stopping Internet Structure Data Collection Worker..."

    if [ -f "$PID_FILE" ]; then
        WORKER_PID=$(cat "$PID_FILE")

        if kill -0 "$WORKER_PID" 2>/dev/null; then
            log_info "Terminating worker process $WORKER_PID..."

            # Try
