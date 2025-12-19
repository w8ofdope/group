#' Data Collection Module
#'
#' This module provides functions for collecting Internet structure data
#' from public sources like DB-IP and MaxMind GeoLite databases.
#'
#' @name data_collection
#' @import httr
#' @import dplyr
#' @import readr
#' @import stringr
NULL

#' Collect DB-IP Data
#'
#' Downloads and processes IP-to-ASN mapping data from DB-IP service.
#'
#' @param url Character string. URL to DB-IP CSV data (default: free database)
#' @param save_path Character string. Path to save downloaded file (optional)
#' @return A data.frame with IP ranges, ASN, and organization information
#' @export
#' @examples
#' \dontrun{
#' dbip_data <- collect_dbip_data()
#' head(dbip_data)
#' }
collect_dbip_data <- function(url = "https://download.db-ip.com/free/dbip-asn-lite-2023-12.csv.gz",
                             save_path = NULL) {
  tryCatch({
    message("Downloading DB-IP data...")

    # Create temporary file if no save path provided
    if (is.null(save_path)) {
      temp_file <- tempfile(fileext = ".csv.gz")
    } else {
      temp_file <- save_path
    }

    # Download the file
    response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))

    if (httr::status_code(response) != 200) {
      stop("Failed to download DB-IP data: HTTP ", httr::status_code(response))
    }

    message("Parsing DB-IP data...")

    # Read and parse the CSV data
    data <- readr::read_csv(temp_file,
                           col_names = c("start_ip", "end_ip", "asn", "organization"),
                           col_types = "cccc",
                           progress = FALSE)

    # Clean and process the data
    processed_data <- data %>%
      dplyr::mutate(
        asn = stringr::str_extract(asn, "\\d+") %>% as.numeric(),
        organization = stringr::str_trim(organization),
        start_ip_numeric = ip_to_numeric(start_ip),
        end_ip_numeric = ip_to_numeric(end_ip)
      ) %>%
      dplyr::filter(!is.na(asn))

    message(sprintf("Successfully collected %d IP ranges from DB-IP", nrow(processed_data)))

    return(processed_data)

  }, error = function(e) {
    stop("Error collecting DB-IP data: ", e$message)
  })
}

#' Collect MaxMind GeoLite Data
#'
#' Downloads and processes IP geolocation data from MaxMind GeoLite database.
#'
#' @param license_key Character string. MaxMind license key (required for full access)
#' @param save_path Character string. Path to save downloaded file (optional)
#' @return A data.frame with IP ranges and geographic information
#' @export
#' @examples
#' \dontrun{
#' maxmind_data <- collect_maxmind_data(license_key = "your_key")
#' head(maxmind_data)
#' }
collect_maxmind_data <- function(license_key = NULL,
                                save_path = NULL) {
  tryCatch({
    if (is.null(license_key)) {
      warning("No license key provided. Using free GeoLite2-Country-CSV database.")
      url <- "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-Country-CSV&license_key=&suffix=zip"
    } else {
      url <- sprintf("https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-Country-CSV&license_key=%s&suffix=zip", license_key)
    }

    message("Downloading MaxMind GeoLite data...")

    # Create temporary file if no save path provided
    if (is.null(save_path)) {
      temp_file <- tempfile(fileext = ".zip")
    } else {
      temp_file <- save_path
    }

    # Download the file
    response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))

    if (httr::status_code(response) != 200) {
      stop("Failed to download MaxMind data: HTTP ", httr::status_code(response))
    }

    message("Extracting and parsing MaxMind data...")

    # Extract the ZIP file
    temp_dir <- tempdir()
    utils::unzip(temp_file, exdir = temp_dir)

    # Find the blocks file
    blocks_file <- list.files(temp_dir, pattern = "GeoLite2-Country-Blocks.*\\.csv$", recursive = TRUE, full.names = TRUE)

    if (length(blocks_file) == 0) {
      stop("Could not find GeoLite2-Country-Blocks file in downloaded archive")
    }

    # Read the blocks data
    blocks_data <- readr::read_csv(blocks_file[1],
                                  col_types = "cccc",
                                  progress = FALSE)

    # Find and read locations file
    locations_file <- list.files(temp_dir, pattern = "GeoLite2-Country-Locations.*\\.csv$", recursive = TRUE, full.names = TRUE)

    if (length(locations_file) > 0) {
      locations_data <- readr::read_csv(locations_file[1],
                                       col_types = "cccccccc",
                                       progress = FALSE)

      # Merge blocks with locations
      merged_data <- blocks_data %>%
        dplyr::left_join(locations_data, by = "geoname_id") %>%
        dplyr::select(network, country_code = country_iso_code,
                     country_name, continent_code, continent_name) %>%
        dplyr::mutate(
          start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
          prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
          start_ip_numeric = ip_to_numeric(start_ip)
        )

      message(sprintf("Successfully collected %d IP ranges from MaxMind", nrow(merged_data)))

      return(merged_data)
    } else {
      # Return blocks data only if locations not available
      blocks_data <- blocks_data %>%
        dplyr::mutate(
          start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
          prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
          start_ip_numeric = ip_to_numeric(start_ip)
        )

      message(sprintf("Successfully collected %d IP ranges from MaxMind (locations not available)", nrow(blocks_data)))

      return(blocks_data)
    }

  }, error = function(e) {
    stop("Error collecting MaxMind data: ", e$message)
  })
}

#' Download GeoLite ASN Data
#'
#' Downloads the MaxMind GeoLite ASN database for IP-to-ASN mapping.
#'
#' @param license_key Character string. MaxMind license key (optional)
#' @param save_path Character string. Path to save downloaded file (optional)
#' @return A data.frame with IP ranges and ASN information
#' @export
download_geolite_asn <- function(license_key = NULL, save_path = NULL) {
  tryCatch({
    if (is.null(license_key)) {
      url <- "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-ASN-CSV&license_key=&suffix=zip"
    } else {
      url <- sprintf("https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-ASN-CSV&license_key=%s&suffix=zip", license_key)
    }

    message("Downloading MaxMind GeoLite ASN data...")

    # Create temporary file if no save path provided
    if (is.null(save_path)) {
      temp_file <- tempfile(fileext = ".zip")
    } else {
      temp_file <- save_path
    }

    # Download the file
    response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))

    if (httr::status_code(response) != 200) {
      stop("Failed to download GeoLite ASN data: HTTP ", httr::status_code(response))
    }

    # Extract and parse the data
    temp_dir <- tempdir()
    utils::unzip(temp_file, exdir = temp_dir)

    asn_file <- list.files(temp_dir, pattern = "GeoLite2-ASN-Blocks.*\\.csv$", recursive = TRUE, full.names = TRUE)

    if (length(asn_file) == 0) {
      stop("Could not find GeoLite2-ASN-Blocks file in downloaded archive")
    }

    asn_data <- readr::read_csv(asn_file[1],
                               col_types = "ccc",
                               progress = FALSE) %>%
      dplyr::mutate(
        start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
        prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
        start_ip_numeric = ip_to_numeric(start_ip),
        autonomous_system_number = stringr::str_extract(autonomous_system_number, "\\d+") %>% as.numeric()
      ) %>%
      dplyr::filter(!is.na(autonomous_system_number))

    message(sprintf("Successfully downloaded %d ASN records from MaxMind", nrow(asn_data)))

    return(asn_data)

  }, error = function(e) {
    stop("Error downloading GeoLite ASN data: ", e$message)
  })
}

#' Parse GeoLite CSV Data
#'
#' Parses downloaded GeoLite CSV files into structured data frames.
#'
#' @param csv_path Character string. Path to the CSV file
#' @param type Character string. Type of GeoLite data ("country", "asn", "city")
#' @return A data.frame with parsed geographic or ASN data
#' @export
parse_geolite_csv <- function(csv_path, type = c("country", "asn", "city")) {
  type <- match.arg(type)

  if (!file.exists(csv_path)) {
    stop("CSV file does not exist: ", csv_path)
  }

  tryCatch({
    message(sprintf("Parsing GeoLite %s data...", type))

    if (type == "asn") {
      data <- readr::read_csv(csv_path,
                             col_names = c("network", "autonomous_system_number", "autonomous_system_organization"),
                             col_types = "ccc",
                             skip = 1,  # Skip header
                             progress = FALSE)

      processed_data <- data %>%
        dplyr::mutate(
          start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
          prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
          start_ip_numeric = ip_to_numeric(start_ip),
          autonomous_system_number = stringr::str_extract(autonomous_system_number, "\\d+") %>% as.numeric()
        ) %>%
        dplyr::filter(!is.na(autonomous_system_number))

    } else if (type == "country") {
      data <- readr::read_csv(csv_path,
                             col_types = "cccc",
                             progress = FALSE)

      processed_data <- data %>%
        dplyr::mutate(
          start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
          prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
          start_ip_numeric = ip_to_numeric(start_ip)
        )

    } else { # city
      data <- readr::read_csv(csv_path,
                             col_types = "cccccccccccccc",
                             progress = FALSE)

      processed_data <- data %>%
        dplyr::mutate(
          start_ip = stringr::str_split(network, "/", simplify = TRUE)[,1],
          prefix_length = as.numeric(stringr::str_split(network, "/", simplify = TRUE)[,2]),
          start_ip_numeric = ip_to_numeric(start_ip)
        )
    }

    message(sprintf("Successfully parsed %d records", nrow(processed_data)))
    return(processed_data)

  }, error = function(e) {
    stop("Error parsing GeoLite CSV: ", e$message)
  })
}

# Helper function to convert IP to numeric
ip_to_numeric <- function(ip) {
  parts <- as.numeric(stringr::str_split(ip, "\\.", simplify = TRUE))
  return(parts[1] * 256^3 + parts[2] * 256^2 + parts[3] * 256 + parts[4])
}
