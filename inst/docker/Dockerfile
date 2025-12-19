# Dockerfile for Internet Structure Explorer R Package
# Multi-stage build for optimal image size

# Base stage with R and system dependencies
FROM rocker/r-ver:4.3.1 AS base

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    git \
    curl \
    wget \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Install R dependencies in stages for better caching

# Stage 1: Core R packages
FROM base AS r-deps-core
RUN install2.r --error --skipinstalled \
    dplyr \
    magrittr \
    tidyr \
    readr \
    httr \
    jsonlite \
    data.table \
    lubridate \
    stringr \
    purrr \
    && rm -rf /tmp/downloaded_packages

# Stage 2: Shiny and visualization packages
FROM r-deps-core AS r-deps-viz
RUN install2.r --error --skipinstalled \
    shiny \
    shinydashboard \
    plotly \
    leaflet \
    DT \
    igraph \
    networkD3 \
    ggplot2 \
    RSQLite \
    DBI \
    && rm -rf /tmp/downloaded_packages

# Stage 3: Additional packages
FROM r-deps-viz AS r-deps-full
RUN install2.r --error --skipinstalled \
    processx \
    xml2 \
    rvest \
    curl \
    openssl \
    yaml \
    testthat \
    knitr \
    rmarkdown \
    devtools \
    roxygen2 \
    pkgdown \
    quarto \
    && rm -rf /tmp/downloaded_packages

# Final application stage
FROM r-deps-full AS app

# Copy package files
COPY DESCRIPTION .
COPY NAMESPACE .
COPY R/ ./R/
COPY man/ ./man/
COPY tests/ ./tests/
COPY vignettes/ ./vignettes/
COPY inst/ ./inst/
COPY data-raw/ ./data-raw/

# Install the package itself
RUN R -e "devtools::install('.', dependencies = FALSE, build_vignettes = FALSE)"

# Create directories for data and logs
RUN mkdir -p /app/data /app/logs /app/output

# Set environment variables
ENV R_ENV=production
ENV R_PROFILE=/app/.Rprofile

# Create .Rprofile for the application
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'))" > .Rprofile
RUN echo "options(shiny.port = 3838)" >> .Rprofile
RUN echo "options(shiny.host = '0.0.0.0')" >> .Rprofile

# Expose port for Shiny app
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Default command to run the Shiny app
CMD ["R", "-e", "internetstructure::run_shiny_app(port = 3838, host = '0.0.0.0')"]
