# Script to prepare demo data for the internetstructure package
# This script downloads sample data and saves it as .rda files for inclusion in the package

library(internetstructure)
library(dplyr)
library(readr)

message("Preparing demo data for internetstructure package...")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# ==============================================================================
# 1. Download and prepare DB-IP demo data
# ==============================================================================

message("Downloading DB-IP demo data...")

tryCatch({
  # Download a small sample of DB-IP data (we'll limit it to first 10,000 records)
  dbip_url <- "https://download.db-ip.com/free/dbip-asn-lite-2023-12.csv.gz"

  # Download to temporary file
  temp_file <- tempfile(fileext = ".csv.gz")
  download.file(dbip_url, temp_file, mode = "wb", quiet = TRUE)

  # Read and limit to sample data (first 5000 records for demo purposes)
  message("Reading and processing DB-IP data...")
  dbip_data <- read_csv(temp_file,
                       col_names = c("start_ip", "end_ip", "asn", "organization"),
                       col_types = "cccc",
                       n_max = 5000,  # Limit for demo
                       progress = FALSE)

  # Process the data
  demo_dbip_data <- dbip_data %>%
    mutate(
      asn = stringr::str_extract(asn, "\\d+") %>% as.numeric(),
      organization = stringr::str_trim(organization),
      start_ip_numeric = ip_to_numeric(start_ip),
      end_ip_numeric = ip_to_numeric(end_ip)
    ) %>%
    filter(!is.na(asn)) %>%
    # Take a diverse sample across different ASNs
    group_by(asn) %>%
    slice(1) %>%
    ungroup() %>%
    head(1000)  # Final demo size

  message(sprintf("Prepared %d DB-IP records for demo", nrow(demo_dbip_data)))

  # Save as .rda file
  save(demo_dbip_data, file = "data/demo_dbip_data.rda",
       compress = "gzip", compression_level = 9)

  message("Saved demo_dbip_data.rda")

}, error = function(e) {
  message("Error downloading DB-IP data: ", e$message)
  message("Creating minimal demo data...")

  # Create minimal demo data if download fails
  demo_dbip_data <- data.frame(
    start_ip = c("8.8.8.0", "1.1.1.0", "208.67.222.0"),
    end_ip = c("8.8.8.255", "1.1.1.255", "208.67.222.255"),
    asn = c(15169, 13335, 3356),
    organization = c("Google LLC", "Cloudflare, Inc.", "Level 3 Communications, Inc."),
    start_ip_numeric = c(ip_to_numeric("8.8.8.0"), ip_to_numeric("1.1.1.0"), ip_to_numeric("208.67.222.0")),
    end_ip_numeric = c(ip_to_numeric("8.8.8.255"), ip_to_numeric("1.1.1.255"), ip_to_numeric("208.67.222.255"))
  )

  save(demo_dbip_data, file = "data/demo_dbip_data.rda")
  message("Saved minimal demo_dbip_data.rda")
})

# ==============================================================================
# 2. Download and prepare MaxMind GeoLite2 demo data
# ==============================================================================

message("Downloading MaxMind GeoLite2 demo data...")

tryCatch({
  # Download GeoLite2-Country CSV (free version)
  geo_url <- "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-Country-CSV&license_key=&suffix=zip"

  temp_zip <- tempfile(fileext = ".zip")
  download.file(geo_url, temp_zip, mode = "wb", quiet = TRUE)

  # Extract the ZIP
  temp_dir <- tempdir()
  unzip(temp_zip, exdir = temp_dir)

  # Find the blocks file
  blocks_file <- list.files(temp_dir, pattern = "GeoLite2-Country-Blocks.*\\.csv$",
                           recursive = TRUE, full.names = TRUE)

  if (length(blocks_file) == 0) {
    stop("Could not find GeoLite2-Country-Blocks file")
  }

  # Read blocks data (limit for demo)
  blocks_data <- read_csv(blocks_file[1],
                         col_types = "cccc",
                         n_max = 10000,
                         progress = FALSE)

  # Find and read locations file
  locations_file <- list.files(temp_dir, pattern = "GeoLite2-Country-Locations.*\\.csv$",
                              recursive = TRUE, full.names = TRUE)

  demo_geo_data <- if (length(locations_file) > 0) {
    locations_data <- read_csv(locations_file[1],
                              col_types = "cccccccc",
                              progress = FALSE)

    # Merge and prepare
    blocks_data %>%
      left_join(locations_data, by = "geoname_id") %>%
      select(network, country_code = country_iso_code,
             country_name, continent_code, continent_name) %>%
      mutate(
        start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
        prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
        start_ip_numeric = ip_to_numeric(start_ip)
      ) %>%
      filter(!is.na(country_code)) %>%
      head(2000)  # Demo size
  } else {
    # Fallback if locations not available
    blocks_data %>%
      mutate(
        start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
        prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
        start_ip_numeric = ip_to_numeric(start_ip)
      ) %>%
      head(2000)
  }

  message(sprintf("Prepared %d geographic records for demo", nrow(demo_geo_data)))

  # Save as .rda file
  save(demo_geo_data, file = "data/demo_geo_data.rda",
       compress = "gzip", compression_level = 9)

  message("Saved demo_geo_data.rda")

}, error = function(e) {
  message("Error downloading MaxMind data: ", e$message)
  message("Creating minimal demo geo data...")

  # Create minimal demo geo data
  demo_geo_data <- data.frame(
    network = c("8.8.8.0/24", "1.1.1.0/24", "208.67.222.0/24"),
    country_code = c("US", "US", "US"),
    country_name = c("United States", "United States", "United States"),
    continent_code = c("NA", "NA", "NA"),
    continent_name = c("North America", "North America", "North America"),
    start_ip = c("8.8.8.0", "1.1.1.0", "208.67.222.0"),
    prefix_length = c(24, 24, 24),
    start_ip_numeric = c(ip_to_numeric("8.8.8.0"), ip_to_numeric("1.1.1.0"), ip_to_numeric("208.67.222.0"))
  )

  save(demo_geo_data, file = "data/demo_geo_data.rda")
  message("Saved minimal demo_geo_data.rda")
})

# ==============================================================================
# 3. Create unified demo data
# ==============================================================================

message("Creating unified demo dataset...")

tryCatch({
  demo_unified_data <- create_as_dataframe(demo_dbip_data, demo_geo_data)

  # Save unified data
  save(demo_unified_data, file = "data/demo_unified_data.rda",
       compress = "gzip", compression_level = 9)

  message(sprintf("Created unified demo dataset with %d records", nrow(demo_unified_data)))
  message("Saved demo_unified_data.rda")

}, error = function(e) {
  message("Error creating unified demo data: ", e$message)
})

# ==============================================================================
# 4. Create demo traceroute data
# ==============================================================================

message("Creating demo traceroute data...")

# Create sample traceroute data for demonstration
demo_traceroute_data <- data.frame(
  hop = 1:8,
  ip_hostname = c("192.168.1.1", "10.0.0.1", "203.0.113.1", "8.8.8.8", "8.8.8.8", "8.8.8.8", "8.8.8.8", "8.8.8.8"),
  rtt1 = c(1.2, 5.4, 23.1, 45.2, 44.8, 45.1, 44.9, 45.3),
  rtt2 = c(1.1, 5.6, 22.8, 45.8, 45.2, 44.7, 45.1, 45.0),
  rtt3 = c(1.3, 5.2, 23.4, 44.9, 45.1, 45.3, 44.8, 45.2),
  avg_rtt = c(1.2, 5.4, 23.1, 45.3, 45.0, 45.0, 44.9, 45.2),
  asn = c(NA, NA, 3356, 15169, 15169, 15169, 15169, 15169),
  target = "8.8.8.8",
  protocol = "icmp",
  timestamp = Sys.time()
)

# Create AS path summary
as_path <- paste(unique(na.omit(demo_traceroute_data$asn)), collapse = " -> ")
demo_traceroute_data$as_path <- as_path
demo_traceroute_data$as_path_length <- length(unique(na.omit(demo_traceroute_data$asn)))

# Save traceroute demo data
save(demo_traceroute_data, file = "data/demo_traceroute_data.rda",
     compress = "gzip", compression_level = 9)

message("Saved demo_traceroute_data.rda")

# ==============================================================================
# Summary
# ==============================================================================

message("\nDemo data preparation completed!")
message("Created files:")
message("- data/demo_dbip_data.rda")
message("- data/demo_geo_data.rda")
message("- data/demo_unified_data.rda")
message("- data/demo_traceroute_data.rda")

# List files
list.files("data", pattern = "*.rda")

message("\nDemo data is ready for inclusion in the package!")
