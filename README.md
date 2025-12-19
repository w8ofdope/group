# Internet Structure Explorer

A comprehensive R package for researching the structure and connectivity of the Internet based on autonomous systems (AS) data, BGP routes, and network traffic analysis.

## Overview

This project investigates the global Internet structure and connectivity by collecting and analyzing data from multiple sources:

- **DB-IP** and **MaxMind GeoLite** databases for IP-to-ASN mappings
- **Traceroute** analysis for network path discovery
- **BGP routing** data for AS relationships
- Interactive **Shiny web application** for data exploration
- **Docker containerization** for easy deployment

## Features

### 📊 Data Collection
- Automated collection from public IP geolocation databases
- Support for multiple data sources (DB-IP, MaxMind, RIPE, CAIDA)
- Incremental updates and data validation
- Background worker for continuous data collection

### 🛣️ Network Analysis
- Traceroute execution and parsing (IPv4/IPv6, multiple protocols)
- AS path extraction and analysis
- Network connectivity metrics calculation
- Geographic distribution analysis

### 🌐 Interactive Visualization
- Shiny web application with multiple dashboards
- Geographic maps showing AS distribution
- Network graphs of AS relationships
- Interactive data tables and charts
- Real-time traceroute exploration

### 🐳 Containerization
- Docker images for all components
- Docker Compose for complete stack deployment
- PostgreSQL database for data storage
- Redis for caching and session management
- Nginx reverse proxy for production deployment

## Installation

### From GitHub (Recommended)

```r
# Install remotes if not already installed
install.packages("remotes")

# Install the package
remotes::install_github("w8ofdope/group", ref = "main")
```

### Manual Installation

```r
# Clone the repository
git clone https://github.com/w8ofdope/group.git
cd group

# Install dependencies and build package
install.packages(c("devtools", "roxygen2"))
devtools::install_deps()
devtools::document()
devtools::install()
```

## Quick Start

### Using Demo Data (Recommended for First Time)

The package includes demo datasets for immediate exploration:

```r
library(internetstructure)

# Load demo data
data(demo_dbip_data)       # IP-ASN mappings
data(demo_geo_data)        # Geographic data
data(demo_unified_data)    # Combined AS data
data(demo_traceroute_data) # Sample traceroute results

# Launch Shiny application with demo data
run_shiny_app(as_data = demo_unified_data)
```

### Basic Usage with Live Data

```r
library(internetstructure)

# Collect IP-ASN data from sources
dbip_data <- collect_dbip_data()
maxmind_data <- collect_maxmind_data()

# Create unified AS dataset
as_data <- create_as_dataframe(dbip_data, maxmind_data)

# Run traceroute analysis
trace_result <- run_traceroute("8.8.8.8")
parsed_trace <- parse_traceroute_output(trace_result)
as_path <- extract_as_path(parsed_trace, dbip_data)

# Launch Shiny application
run_shiny_app(as_data = as_data)
```

### Docker Deployment

```bash
# Clone repository
git clone https://github.com/w8ofdope/group.git
cd group

# Start all services
docker-compose up -d

# View logs
docker-compose logs -f internet-structure-app

# Access application at http://localhost:3838
```

## Project Structure

```
internetstructure/
├── DESCRIPTION                    # Package metadata
├── NAMESPACE                      # Package exports
├── README.md                      # This file
├── docker-compose.yml            # Docker services configuration
├── R/                            # R source code
│   ├── data_collection.R         # Data collection functions
│   ├── traceroute.R              # Traceroute analysis
│   ├── as_data.R                 # AS data management
│   ├── visualization.R           # Shiny application
│   └── utilities.R               # Helper functions
├── man/                          # Documentation
├── tests/                        # Unit tests
├── vignettes/                    # Usage examples
├── data-raw/                     # Raw data processing
└── inst/                         # Additional files
    ├── docker/                   # Docker configuration
    │   ├── Dockerfile           # Main application container
    │   ├── Dockerfile.worker    # Worker container
    │   ├── nginx.conf           # Reverse proxy config
    │   ├── init-db.sql          # Database initialization
    │   ├── worker.R             # Worker script
    │   └── worker.sh            # Worker launcher
    └── ...
```

## Data Sources

### Primary Data Sources
- **DB-IP**: Free IP-to-ASN mapping database
- **MaxMind GeoLite2**: Geographic location data
- **RIPE NCC**: Regional Internet Registry data
- **CAIDA**: AS relationship data

### Network Analysis
- **Traceroute**: Network path discovery
- **BGP Routing Tables**: AS interconnection data
- **RIPE RIS/Atlas**: Active BGP measurement data

## API Reference

### Data Collection
- `collect_dbip_data()` - Download DB-IP ASN data
- `collect_maxmind_data()` - Download MaxMind geographic data
- `download_geolite_asn()` - Download MaxMind ASN data

### Network Analysis
- `run_traceroute()` - Execute traceroute to target
- `parse_traceroute_output()` - Parse traceroute results
- `extract_as_path()` - Extract AS path from traceroute data
- `batch_traceroute()` - Run traceroute to multiple targets

### Data Management
- `create_as_dataframe()` - Create unified AS data frame
- `merge_as_data()` - Merge multiple data sources
- `save_as_data()` / `load_as_data()` - Data persistence
- `get_as_info()` - Retrieve AS information

### Visualization
- `create_shiny_app()` - Create Shiny application
- `run_shiny_app()` - Launch Shiny application
- `create_network_plot()` - Static network visualization
- `create_geographic_map()` - Geographic visualization

### Utilities
- `validate_ip()` - IP address validation
- `ip_to_asn()` - IP to ASN lookup
- `calculate_connectivity_metrics()` - Network metrics
- `format_as_info()` - Format AS information

## Configuration

### Environment Variables

For Docker deployment:

```bash
# Database configuration
DB_HOST=internet-structure-db
DB_PORT=5432
DB_NAME=internet_structure
DB_USER=internet_user
DB_PASSWORD=internet_pass_2024

# Redis configuration (optional)
REDIS_HOST=internet-structure-redis
REDIS_PORT=6379

# Shiny application
SHINY_HOST=0.0.0.0
SHINY_PORT=3838
```

### MaxMind License Key

For full MaxMind data access, set your license key:

```r
options(maxmind_license_key = "your_license_key_here")
```

## Development

### Setting up Development Environment

```r
# Install development dependencies
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Build documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/test-data-collection.R")
```

### Building Package

```r
# Build source package
devtools::build()

# Build binary package
devtools::build(binary = TRUE)

# Check package
devtools::check()
```

## Docker Services

### Available Services

1. **internet-structure-app**: Main Shiny application
2. **internet-structure-db**: PostgreSQL database
3. **internet-structure-redis**: Redis cache (optional)
4. **internet-structure-nginx**: Nginx reverse proxy (production)
5. **internet-structure-worker**: Background data collection worker

### Service Management

```bash
# Start all services
docker-compose up -d

# Start specific service
docker-compose up -d internet-structure-app

# View service logs
docker-compose logs internet-structure-app

# Stop services
docker-compose down

# Rebuild and restart
docker-compose up -d --build
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/new-feature`)
5. Create a Pull Request

### Code Style

- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Use `roxygen2` for documentation
- Write unit tests for new functions
- Update documentation for API changes

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use this package in your research, please cite:

```bibtex
@software{internetstructure,
  title = {Internet Structure Explorer: R Package for Internet Connectivity Analysis},
  author = {Research Group},
  url = {https://github.com/w8ofdope/group},
  year = {2024}
}
```

## Support

- **Issues**: [GitHub Issues](https://github.com/w8ofdope/group/issues)
- **Documentation**: [Package Documentation](https://w8ofdope.github.io/group/)
- **Wiki**: [Project Wiki](https://github.com/w8ofdope/group/wiki)

## Acknowledgments

- **DB-IP** for providing free IP geolocation data
- **MaxMind** for GeoLite2 databases
- **RIPE NCC** for Internet measurement data
- **CAIDA** for Internet topology research
- **R Community** for excellent packages and tools

---

**Note**: This package respects the terms of service of all data providers and implements rate limiting and caching to minimize server load.
