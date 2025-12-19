#' Utility Functions Module
#'
#' This module provides utility functions for IP address manipulation,
#' validation, ASN lookups, and connectivity analysis.
#'
#' @name utilities
#' @import stringr
#' @import dplyr
#' @import purrr
NULL

#' Validate IP Address
#'
#' Checks if a given string is a valid IPv4 or IPv6 address.
#'
#' @param ip Character string. IP address to validate
#' @param version Character string. IP version to check ("ipv4", "ipv6", or "both")
#' @return Logical. TRUE if valid, FALSE otherwise
#' @export
#' @examples
#' validate_ip("192.168.1.1")  # TRUE
#' validate_ip("256.1.1.1")    # FALSE
#' validate_ip("::1")          # TRUE for IPv6
validate_ip <- function(ip, version = c("ipv4", "ipv6", "both")) {
  version <- match.arg(version)

  if (is.na(ip) || ip == "") return(FALSE)

  # IPv4 validation
  ipv4_pattern <- "^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.){3}(25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)$"

  # IPv6 validation (simplified)
  ipv6_pattern <- "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::1$|^::$"

  is_ipv4 <- stringr::str_detect(ip, ipv4_pattern)
  is_ipv6 <- stringr::str_detect(ip, ipv6_pattern)

  switch(version,
         "ipv4" = is_ipv4,
         "ipv6" = is_ipv6,
         "both" = is_ipv4 || is_ipv6)
}

#' Convert IP Address to Numeric
#'
#' Converts an IPv4 address to its numeric representation.
#'
#' @param ip Character string. IPv4 address
#' @return Numeric. Numeric representation of the IP address
#' @export
#' @examples
#' ip_to_numeric("192.168.1.1")  # 3232235777
ip_to_numeric <- function(ip) {
  if (is.na(ip) || ip == "") return(NA_real_)
  if (!validate_ip(ip, "ipv4")) return(NA_real_)

  parts <- as.numeric(stringr::str_split(ip, "\\.", simplify = TRUE))
  if (length(parts) != 4) return(NA_real_)

  return(parts[1] * 256^3 + parts[2] * 256^2 + parts[3] * 256 + parts[4])
}

#' Convert Numeric to IP Address
#'
#' Converts a numeric representation back to an IPv4 address.
#'
#' @param num Numeric. Numeric representation of an IP address
#' @return Character string. IPv4 address
#' @export
#' @examples
#' numeric_to_ip(3232235777)  # "192.168.1.1"
numeric_to_ip <- function(num) {
  if (is.na(num) || num < 0 || num > 4294967295) return(NA_character_)

  parts <- c()
  for (i in 3:0) {
    parts <- c(parts, floor(num / 256^i))
    num <- num %% 256^i
  }

  paste(parts, collapse = ".")
}

#' Check if IP is in Range
#'
#' Checks if an IP address falls within a given IP range.
#'
#' @param ip Character string. IP address to check
#' @param start_ip Character string. Start of the IP range
#' @param end_ip Character string. End of the IP range (optional, can use CIDR notation)
#' @return Logical. TRUE if IP is in range, FALSE otherwise
#' @export
#' @examples
#' ip_in_range("192.168.1.50", "192.168.1.0", "192.168.1.255")  # TRUE
ip_in_range <- function(ip, start_ip, end_ip = NULL) {
  if (is.na(ip) || !validate_ip(ip, "ipv4")) return(FALSE)

  ip_num <- ip_to_numeric(ip)
  start_num <- ip_to_numeric(start_ip)

  if (is.na(ip_num) || is.na(start_num)) return(FALSE)

  # If end_ip is provided, use it directly
  if (!is.null(end_ip)) {
    end_num <- ip_to_numeric(end_ip)
    if (is.na(end_num)) return(FALSE)
    return(ip_num >= start_num && ip_num <= end_num)
  }

  # Check if start_ip is in CIDR notation
  if (stringr::str_detect(start_ip, "/")) {
    cidr_parts <- stringr::str_split(start_ip, "/", simplify = TRUE)
    network <- cidr_parts[1]
    prefix <- as.numeric(cidr_parts[2])

    if (is.na(prefix) || prefix < 0 || prefix > 32) return(FALSE)

    network_num <- ip_to_numeric(network)
    if (is.na(network_num)) return(FALSE)

    # Calculate subnet mask
    mask <- bitwShiftL(bitwNot(0), 32 - prefix)
    network_start <- bitwAnd(network_num, mask)
    network_end <- bitwOr(network_start, bitwNot(mask))

    return(ip_num >= network_start && ip_num <= network_end)
  }

  # Single IP match
  return(ip_num == start_num)
}

#' IP to ASN Lookup
#'
#' Finds the Autonomous System Number (ASN) for a given IP address
#' using IP-to-ASN mapping data.
#'
#' @param ip Character string. IP address to look up
#' @param ip_asn_data Data.frame. IP-to-ASN mapping data
#' @return Numeric. ASN for the IP address, or NA if not found
#' @export
#' @examples
#' \dontrun{
#' asn <- ip_to_asn("8.8.8.8", dbip_data)
#' }
ip_to_asn <- function(ip, ip_asn_data) {
  if (is.na(ip) || !validate_ip(ip, "ipv4")) return(NA)
  if (nrow(ip_asn_data) == 0) return(NA)

  ip_num <- ip_to_numeric(ip)
  if (is.na(ip_num)) return(NA)

  # Find matching IP range
  # Look for the most specific range (smallest range containing the IP)
  matches <- ip_asn_data %>%
    dplyr::filter(
      ip_num >= start_ip_numeric,
      ip_num <= end_ip_numeric
    ) %>%
    dplyr::mutate(range_size = end_ip_numeric - start_ip_numeric) %>%
    dplyr::arrange(range_size)  # Prefer smaller ranges

  if (nrow(matches) > 0) {
    return(matches$asn[1])
  }

  return(NA)
}

#' Get AS Relationships
#'
#' Retrieves Autonomous System relationship data from CAIDA or similar sources.
#' This is a placeholder for actual relationship data collection.
#'
#' @param asns Numeric vector. ASNs to get relationships for
#' @param source Character string. Data source ("caida", "ripe", etc.)
#' @return Data.frame with AS relationships (peer, provider, customer)
#' @export
#' @examples
#' \dontrun{
#' relationships <- get_as_relationships(c(15169, 3356))
#' }
get_as_relationships <- function(asns, source = c("caida", "ripe", "manual")) {
  source <- match.arg(source)

  tryCatch({
    message(sprintf("Fetching AS relationships from %s...", source))

    # Placeholder for actual relationship data
    # In a real implementation, this would query APIs or databases

    if (source == "caida") {
      # CAIDA AS Relationships
      # This would download and parse CAIDA relationship data
      warning("CAIDA relationship data download not implemented yet")

    } else if (source == "ripe") {
      # RIPE RIS/Atlas data
      # This would query RIPE's routing data
      warning("RIPE relationship data not implemented yet")

    } else {
      # Manual or cached data
      warning("Manual relationship data not available")
    }

    # Return empty data frame for now
    data.frame(
      asn1 = numeric(),
      asn2 = numeric(),
      relationship = character(),  # "peer", "provider", "customer"
      source = character(),
      last_updated = as.POSIXct(character())
    )

  }, error = function(e) {
    warning("Error fetching AS relationships: ", e$message)
    return(data.frame())
  })
}

#' Calculate Connectivity Metrics
#'
#' Calculates various network connectivity metrics from AS and traceroute data.
#'
#' @param as_data Data.frame. Unified AS data
#' @param traceroute_data List. Traceroute results (optional)
#' @return List with connectivity metrics
#' @export
#' @examples
#' \dontrun{
#' metrics <- calculate_connectivity_metrics(as_data, traceroute_results)
#' }
calculate_connectivity_metrics <- function(as_data, traceroute_data = NULL) {
  if (nrow(as_data) == 0) {
    warning("No AS data available for connectivity analysis")
    return(list())
  }

  tryCatch({
    message("Calculating connectivity metrics...")

    metrics <- list()

    # Basic AS metrics
    metrics$total_asns <- length(unique(as_data$asn))
    metrics$total_ip_ranges <- nrow(as_data)
    metrics$total_ip_space <- sum(as_data$ip_count, na.rm = TRUE)

    # Geographic distribution
    metrics$countries_with_asns <- length(unique(na.omit(as_data$country_code)))
    metrics$continents_represented <- length(unique(na.omit(as_data$continent_code)))

    # AS concentration (top ASNs by IP space)
    asn_ip_space <- as_data %>%
      dplyr::group_by(asn, organization) %>%
      dplyr::summarise(total_ip_space = sum(ip_count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total_ip_space))

    metrics$top_asn_concentration <- asn_ip_space %>%
      dplyr::mutate(cumulative_percent = cumsum(total_ip_space) / sum(total_ip_space) * 100)

    # Traceroute-based metrics
    if (!is.null(traceroute_data) && length(traceroute_data) > 0) {
      trace_metrics <- calculate_traceroute_metrics(traceroute_data)
      metrics <- c(metrics, trace_metrics)
    }

    # Network density metrics
    metrics$asn_density_by_country <- as_data %>%
      dplyr::filter(!is.na(country_code)) %>%
      dplyr::group_by(country_code) %>%
      dplyr::summarise(
        asn_count = length(unique(asn)),
        ip_ranges = dplyr::n(),
        density = asn_count / ip_ranges,
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(density))

    message("Connectivity metrics calculated successfully")
    return(metrics)

  }, error = function(e) {
    warning("Error calculating connectivity metrics: ", e$message)
    return(list())
  })
}

#' Calculate Traceroute-based Metrics
#'
#' Internal function to calculate metrics from traceroute data.
#'
#' @param traceroute_data List. Traceroute results
#' @return List with traceroute metrics
calculate_traceroute_metrics <- function(traceroute_data) {
  metrics <- list()

  tryCatch({
    # Extract path information
    paths <- list()
    avg_hops <- c()
    avg_rtt <- c()

    for (trace in traceroute_data) {
      if (!is.null(trace) && is.data.frame(trace) && nrow(trace) > 0) {
        paths <- c(paths, list(trace))
        avg_hops <- c(avg_hops, max(trace$hop, na.rm = TRUE))
        avg_rtt <- c(avg_rtt, mean(trace$avg_rtt, na.rm = TRUE))
      }
    }

    metrics$total_traceroutes <- length(paths)
    metrics$average_hops <- mean(avg_hops, na.rm = TRUE)
    metrics$average_rtt <- mean(avg_rtt, na.rm = TRUE)
    metrics$hop_distribution <- table(avg_hops)
    metrics$rtt_distribution <- quantile(avg_rtt, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

    # AS path analysis
    as_paths <- list()
    for (trace in paths) {
      if ("asn" %in% names(trace)) {
        path_asns <- unique(na.omit(trace$asn))
        if (length(path_asns) > 0) {
          as_paths <- c(as_paths, list(path_asns))
        }
      }
    }

    if (length(as_paths) > 0) {
      path_lengths <- sapply(as_paths, length)
      metrics$average_as_path_length <- mean(path_lengths, na.rm = TRUE)
      metrics$unique_asns_in_paths <- length(unique(unlist(as_paths)))
    }

    return(metrics)

  }, error = function(e) {
    warning("Error calculating traceroute metrics: ", e$message)
    return(list())
  })
}

#' Generate Random IP Addresses
#'
#' Generates random valid IPv4 addresses for testing purposes.
#'
#' @param n Integer. Number of IP addresses to generate
#' @param ranges List. Optional IP ranges to generate within
#' @return Character vector of random IP addresses
#' @export
#' @examples
#' random_ips <- generate_random_ips(10)
generate_random_ips <- function(n = 1, ranges = NULL) {
  if (n <= 0) return(character())

  tryCatch({
    if (is.null(ranges)) {
      # Generate completely random IPs
      ips <- character(n)
      for (i in 1:n) {
        parts <- sample(0:255, 4, replace = TRUE)
        ips[i] <- paste(parts, collapse = ".")
      }
    } else {
      # Generate IPs within specified ranges
      # This is a simplified implementation
      ips <- character(n)
      for (i in 1:n) {
        range_idx <- sample(1:length(ranges), 1)
        range <- ranges[[range_idx]]

        start_num <- ip_to_numeric(range$start)
        end_num <- ip_to_numeric(range$end)

        if (!is.na(start_num) && !is.na(end_num)) {
          random_num <- sample(start_num:end_num, 1)
          ips[i] <- numeric_to_ip(random_num)
        } else {
          # Fallback to random IP
          parts <- sample(0:255, 4, replace = TRUE)
          ips[i] <- paste(parts, collapse = ".")
        }
      }
    }

    return(ips)

  }, error = function(e) {
    warning("Error generating random IPs: ", e$message)
    return(character())
  })
}

#' Validate ASN
#'
#' Checks if an ASN (Autonomous System Number) is valid.
#'
#' @param asn Numeric or character. ASN to validate
#' @return Logical. TRUE if valid, FALSE otherwise
#' @export
#' @examples
#' validate_asn(15169)  # TRUE (Google)
#' validate_asn(0)      # FALSE
#' validate_asn(4294967295)  # TRUE (max 32-bit ASN)
validate_asn <- function(asn) {
  if (is.na(asn)) return(FALSE)

  # Convert to numeric if character
  if (is.character(asn)) {
    asn <- suppressWarnings(as.numeric(asn))
    if (is.na(asn)) return(FALSE)
  }

  # ASN ranges: 1-4294967295 (32-bit)
  return(asn >= 1 && asn <= 4294967295 && asn == floor(asn))
}

#' Calculate CIDR Prefix Length
#'
#' Calculates the CIDR prefix length for an IP range.
#'
#' @param start_ip Character string. Start IP address
#' @param end_ip Character string. End IP address
#' @return Integer. CIDR prefix length (0-32)
#' @export
#' @examples
#' calculate_cidr_prefix("192.168.1.0", "192.168.1.255")  # 24
calculate_cidr_prefix <- function(start_ip, end_ip) {
  start_num <- ip_to_numeric(start_ip)
  end_num <- ip_to_numeric(end_ip)

  if (is.na(start_num) || is.na(end_num)) return(NA)

  # Calculate number of bits needed
  ip_count <- end_num - start_num + 1
  bits_needed <- ceiling(log2(ip_count))

  # CIDR prefix is 32 - bits_needed
  prefix <- 32 - bits_needed

  # Ensure valid range
  max(0, min(32, prefix))
}

#' Format AS Information
#'
#' Formats AS information into a human-readable string.
#'
#' @param asn Numeric. ASN
#' @param organization Character string. Organization name
#' @param country Character string. Country code
#' @return Character string. Formatted AS information
#' @export
#' @examples
#' format_as_info(15169, "Google LLC", "US")  # "AS15169 (Google LLC, US)"
format_as_info <- function(asn, organization = NULL, country = NULL) {
  info <- paste0("AS", asn)

  if (!is.null(organization) && !is.na(organization)) {
    info <- paste0(info, " (", organization)
    if (!is.null(country) && !is.na(country)) {
      info <- paste0(info, ", ", country)
    }
    info <- paste0(info, ")")
  } else if (!is.null(country) && !is.na(country)) {
    info <- paste0(info, " (", country, ")")
  }

  return(info)
}
