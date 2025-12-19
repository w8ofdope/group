#' Autonomous System Data Representation Module
#'
#' This module provides functions for creating unified data frames
#' containing Autonomous System information, IP subnets, and geographic data.
#'
#' @name as_data
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import DBI
#' @import RSQLite
#' @import jsonlite
NULL

#' Create Unified AS Data Frame
#'
#' Creates a unified data frame combining AS information, IP subnets,
#' and geographic location data from multiple sources.
#'
#' @param ip_asn_data Data.frame. IP-to-ASN mapping data
#' @param geo_data Data.frame. Geographic location data (optional)
#' @param as_info_data Data.frame. Additional AS information (optional)
#' @return A unified data.frame with AS information
#' @export
#' @examples
#' \dontrun{
#' dbip_data <- collect_dbip_data()
#' maxmind_data <- collect_maxmind_data()
#' unified <- create_as_dataframe(dbip_data, maxmind_data)
#' head(unified)
#' }
create_as_dataframe <- function(ip_asn_data, geo_data = NULL, as_info_data = NULL) {
  tryCatch({
    message("Creating unified AS data frame...")

    if (nrow(ip_asn_data) == 0) {
      stop("IP-ASN data is empty")
    }

    # Start with IP-ASN data as the base
    unified_data <- ip_asn_data

    # Rename columns for consistency
    unified_data <- unified_data %>%
      dplyr::rename(
        asn = if("asn" %in% names(.)) "asn" else if("autonomous_system_number" %in% names(.)) "autonomous_system_number" else names(.)[grep("asn|autonomous", names(.), ignore.case = TRUE)[1]],
        organization = if("organization" %in% names(.)) "organization" else if("autonomous_system_organization" %in% names(.)) "autonomous_system_organization" else "organization",
        start_ip = if("start_ip" %in% names(.)) "start_ip" else names(.)[grep("start.*ip", names(.), ignore.case = TRUE)[1]],
        end_ip = if("end_ip" %in% names(.)) "end_ip" else "end_ip"
      )

    # Add geographic data if provided
    if (!is.null(geo_data) && nrow(geo_data) > 0) {
      message("Merging geographic data...")

      # Prepare geo data for merging
      geo_prepared <- geo_data %>%
        dplyr::select(
          start_ip,
          country_code = if("country_code" %in% names(.)) "country_code" else if("country_iso_code" %in% names(.)) "country_iso_code" else "country_code",
          country_name = if("country_name" %in% names(.)) "country_name" else "country_name",
          continent_code = if("continent_code" %in% names(.)) "continent_code" else "continent_code",
          continent_name = if("continent_name" %in% names(.)) "continent_name" else "continent_name",
          city_name = if("city_name" %in% names(.)) "city_name" else NA,
          latitude = if("latitude" %in% names(.)) "latitude" else NA,
          longitude = if("longitude" %in% names(.)) "longitude" else NA
        ) %>%
        dplyr::mutate(
          start_ip_numeric = ip_to_numeric(start_ip)
        )

      # Merge geographic data
      unified_data <- unified_data %>%
        dplyr::left_join(geo_prepared, by = c("start_ip", "start_ip_numeric"))
    }

    # Add additional AS information if provided
    if (!is.null(as_info_data) && nrow(as_info_data) > 0) {
      message("Merging additional AS information...")

      # Prepare AS info data
      as_info_prepared <- as_info_data %>%
        dplyr::select(
          asn,
          as_description = if("description" %in% names(.)) "description" else if("as_description" %in% names(.)) "as_description" else "as_description",
          as_type = if("type" %in% names(.)) "type" else if("as_type" %in% names(.)) "as_type" else "as_type",
          source = if("source" %in% names(.)) "source" else "source"
        )

      # Merge AS information
      unified_data <- unified_data %>%
        dplyr::left_join(as_info_prepared, by = "asn")
    }

    # Calculate subnet information
    unified_data <- unified_data %>%
      dplyr::mutate(
        subnet_mask = calculate_subnet_mask(start_ip, end_ip),
        prefix_length = stringr::str_extract(subnet_mask, "\\d+$") %>% as.numeric(),
        ip_count = calculate_ip_count(start_ip_numeric, end_ip_numeric)
      )

    # Add metadata
    unified_data <- unified_data %>%
      dplyr::mutate(
        data_source = "unified_as_data",
        created_at = Sys.time(),
        last_updated = Sys.time()
      )

    # Remove rows with missing essential data
    unified_data <- unified_data %>%
      dplyr::filter(!is.na(asn), !is.na(start_ip))

    # Sort by ASN and IP range
    unified_data <- unified_data %>%
      dplyr::arrange(asn, start_ip_numeric)

    message(sprintf("Created unified AS data frame with %d records for %d unique ASNs",
                   nrow(unified_data), length(unique(unified_data$asn))))

    return(unified_data)

  }, error = function(e) {
    stop("Error creating unified AS data frame: ", e$message)
  })
}

#' Merge Multiple AS Data Sources
#'
#' Merges data from multiple sources (DB-IP, MaxMind, etc.) into a single
#' comprehensive AS data frame, handling duplicates and conflicts.
#'
#' @param data_list List of data.frames containing AS data from different sources
#' @param source_names Character vector. Names of data sources (optional)
#' @param priority_order Character vector. Order of priority for conflicting data (optional)
#' @return A merged data.frame with resolved conflicts
#' @export
#' @examples
#' \dontrun{
#' dbip <- collect_dbip_data()
#' maxmind_asn <- download_geolite_asn()
#' merged <- merge_as_data(list(dbip, maxmind_asn), c("dbip", "maxmind"))
#' }
merge_as_data <- function(data_list, source_names = NULL, priority_order = NULL) {
  if (!is.list(data_list) || length(data_list) == 0) {
    stop("data_list must be a non-empty list of data frames")
  }

  tryCatch({
    message("Merging multiple AS data sources...")

    # Set default source names if not provided
    if (is.null(source_names)) {
      source_names <- paste0("source_", seq_along(data_list))
    }

    if (length(source_names) != length(data_list)) {
      stop("source_names length must match data_list length")
    }

    # Standardize column names for each data source
    standardized_data <- list()

    for (i in seq_along(data_list)) {
      data <- data_list[[i]]
      source_name <- source_names[i]

      if (nrow(data) == 0) next

      # Standardize column names
      standardized <- data %>%
        dplyr::rename_with(
          ~ dplyr::case_when(
            stringr::str_detect(.x, "asn|autonomous.*system.*number") ~ "asn",
            stringr::str_detect(.x, "organization|autonomous.*system.*org") ~ "organization",
            stringr::str_detect(.x, "^start.*ip$") ~ "start_ip",
            stringr::str_detect(.x, "^end.*ip$") ~ "end_ip",
            stringr::str_detect(.x, "country.*code|country_iso") ~ "country_code",
            stringr::str_detect(.x, "country.*name") ~ "country_name",
            TRUE ~ .x
          )
        ) %>%
        dplyr::mutate(
          data_source = source_name,
          start_ip_numeric = if("start_ip" %in% names(.)) ip_to_numeric(start_ip) else NA,
          end_ip_numeric = if("end_ip" %in% names(.)) ip_to_numeric(end_ip) else NA
        ) %>%
        dplyr::filter(!is.na(asn))

      standardized_data[[i]] <- standardized
    }

    # Combine all sources
    combined_data <- dplyr::bind_rows(standardized_data)

    # Handle duplicates based on IP range
    # Group by IP range and ASN, keeping the most complete record
    deduplicated <- combined_data %>%
      dplyr::group_by(start_ip, end_ip, asn) %>%
      dplyr::arrange(
        dplyr::desc(!is.na(organization)),
        dplyr::desc(!is.na(country_code)),
        data_source
      ) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Apply priority order if specified
    if (!is.null(priority_order)) {
      deduplicated <- deduplicated %>%
        dplyr::mutate(
          priority_score = match(data_source, priority_order)
        ) %>%
        dplyr::arrange(priority_score, asn, start_ip_numeric) %>%
        dplyr::select(-priority_score)
    }

    message(sprintf("Merged %d data sources into %d unique AS records",
                   length(data_list), nrow(deduplicated)))

    return(deduplicated)

  }, error = function(e) {
    stop("Error merging AS data: ", e$message)
  })
}

#' Save AS Data to File or Database
#'
#' Saves the unified AS data frame to various formats: CSV, RDS, SQLite database.
#'
#' @param as_data Data.frame. Unified AS data to save
#' @param file_path Character string. Path to save the file
#' @param format Character string. Format to save ("csv", "rds", "sqlite")
#' @param table_name Character string. Table name for SQLite (default: "as_data")
#' @param append Logical. Whether to append to existing SQLite table
#' @export
#' @examples
#' \dontrun{
#' as_data <- create_as_dataframe(dbip_data)
#' save_as_data(as_data, "as_data.csv", "csv")
#' save_as_data(as_data, "as_database.db", "sqlite")
#' }
save_as_data <- function(as_data, file_path, format = c("csv", "rds", "sqlite"),
                        table_name = "as_data", append = FALSE) {
  format <- match.arg(format)

  if (nrow(as_data) == 0) {
    warning("No data to save")
    return(invisible(NULL))
  }

  tryCatch({
    message(sprintf("Saving AS data to %s format...", format))

    if (format == "csv") {
      readr::write_csv(as_data, file_path)
      message(sprintf("Saved %d records to %s", nrow(as_data), file_path))

    } else if (format == "rds") {
      saveRDS(as_data, file_path)
      message(sprintf("Saved %d records to %s", nrow(as_data), file_path))

    } else if (format == "sqlite") {
      # Connect to SQLite database
      con <- DBI::dbConnect(RSQLite::SQLite(), file_path)

      # Write data to table
      DBI::dbWriteTable(con, table_name, as_data, overwrite = !append, append = append)

      # Create indexes for better query performance
      DBI::dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_asn ON %s (asn)", table_name))
      DBI::dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_ip_start ON %s (start_ip_numeric)", table_name))
      DBI::dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_ip_end ON %s (end_ip_numeric)", table_name))

      DBI::dbDisconnect(con)

      message(sprintf("Saved %d records to SQLite database %s (table: %s)",
                     nrow(as_data), file_path, table_name))
    }

    return(invisible(NULL))

  }, error = function(e) {
    stop("Error saving AS data: ", e$message)
  })
}

#' Load AS Data from File or Database
#'
#' Loads AS data from various formats: CSV, RDS, SQLite database.
#'
#' @param file_path Character string. Path to the file/database
#' @param format Character string. Format to load ("csv", "rds", "sqlite")
#' @param table_name Character string. Table name for SQLite (default: "as_data")
#' @param query Character string. SQL query for SQLite (optional)
#' @return A data.frame with loaded AS data
#' @export
#' @examples
#' \dontrun{
#' as_data <- load_as_data("as_data.csv", "csv")
#' as_data <- load_as_data("as_database.db", "sqlite")
#' }
load_as_data <- function(file_path, format = c("csv", "rds", "sqlite"),
                        table_name = "as_data", query = NULL) {
  format <- match.arg(format)

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  tryCatch({
    message(sprintf("Loading AS data from %s format...", format))

    if (format == "csv") {
      data <- readr::read_csv(file_path, show_col_types = FALSE)

    } else if (format == "rds") {
      data <- readRDS(file_path)

    } else if (format == "sqlite") {
      con <- DBI::dbConnect(RSQLite::SQLite(), file_path)

      if (!is.null(query)) {
        data <- DBI::dbGetQuery(con, query)
      } else {
        data <- DBI::dbReadTable(con, table_name)
      }

      DBI::dbDisconnect(con)
    }

    message(sprintf("Loaded %d records from %s", nrow(data), file_path))
    return(data)

  }, error = function(e) {
    stop("Error loading AS data: ", e$message)
  })
}

#' Get AS Information
#'
#' Retrieves detailed information about specific Autonomous Systems.
#'
#' @param as_data Data.frame. Unified AS data
#' @param asn Numeric or character. ASN(s) to look up
#' @param include_subnets Logical. Include subnet information
#' @param include_geo Logical. Include geographic information
#' @return A data.frame with AS information
#' @export
#' @examples
#' \dontrun{
#' as_data <- load_as_data("as_database.db", "sqlite")
#' google_info <- get_as_info(as_data, 15169)
#' }
get_as_info <- function(as_data, asn, include_subnets = TRUE, include_geo = TRUE) {
  if (nrow(as_data) == 0) {
    warning("AS data is empty")
    return(data.frame())
  }

  tryCatch({
    message(sprintf("Retrieving information for ASN(s): %s",
                   paste(asn, collapse = ", ")))

    # Filter by ASN
    asn_data <- as_data %>%
      dplyr::filter(asn %in% !!asn)

    if (nrow(asn_data) == 0) {
      warning("No data found for specified ASN(s)")
      return(data.frame())
    }

    # Aggregate information per ASN
    as_summary <- asn_data %>%
      dplyr::group_by(asn) %>%
      dplyr::summarise(
        organization = dplyr::first(na.omit(organization)),
        country_code = dplyr::first(na.omit(country_code)),
        country_name = dplyr::first(na.omit(country_name)),
        continent_code = dplyr::first(na.omit(continent_code)),
        continent_name = dplyr::first(na.omit(continent_name)),
        total_ip_ranges = dplyr::n(),
        total_ip_count = sum(ip_count, na.rm = TRUE),
        data_sources = paste(unique(data_source), collapse = ", "),
        .groups = "drop"
      )

    # Add subnet details if requested
    if (include_subnets) {
      subnet_info <- asn_data %>%
        dplyr::select(asn, start_ip, end_ip, subnet_mask, prefix_length, ip_count) %>%
        dplyr::arrange(asn, start_ip_numeric)

      as_summary <- as_summary %>%
        dplyr::left_join(
          subnet_info %>%
            dplyr::group_by(asn) %>%
            dplyr::summarise(
              subnets = list(dplyr::select(dplyr::cur_data(), -asn)),
              .groups = "drop"
            ),
          by = "asn"
        )
    }

    message(sprintf("Found information for %d ASN(s)", nrow(as_summary)))
    return(as_summary)

  }, error = function(e) {
    stop("Error retrieving AS information: ", e$message)
  })
}

# Helper functions

#' Calculate subnet mask from IP range
#'
#' @param start_ip Character. Start IP address
#' @param end_ip Character. End IP address
#' @return Character string representing subnet mask
calculate_subnet_mask <- function(start_ip, end_ip) {
  # Simple calculation - in a real implementation this would be more sophisticated
  # For now, return a placeholder
  return("/24")  # This is a simplification
}

#' Calculate IP count in range
#'
#' @param start_numeric Numeric. Start IP as numeric
#' @param end_numeric Numeric. End IP as numeric
#' @return Numeric count of IPs in range
calculate_ip_count <- function(start_numeric, end_numeric) {
  if (is.na(start_numeric) || is.na(end_numeric)) return(NA)
  max(1, end_numeric - start_numeric + 1)
}

# Helper function to convert IP to numeric (duplicated from data_collection.R)
ip_to_numeric <- function(ip) {
  if (is.na(ip) || ip == "") return(NA)
  parts <- as.numeric(stringr::str_split(ip, "\\.", simplify = TRUE))
  if (length(parts) != 4) return(NA)
  return(parts[1] * 256^3 + parts[2] * 256^2 + parts[3] * 256 + parts[4])
}
