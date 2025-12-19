#' Traceroute Data Extraction Module
#'
#' This module provides functions for running traceroute commands and
#' extracting AS path information from network routing data.
#'
#' @name traceroute
#' @import processx
#' @import stringr
#' @import dplyr
#' @import purrr
NULL

#' Run Traceroute to Target
#'
#' Executes a traceroute command to a specified IP address or hostname
#' and returns the raw output.
#'
#' @param target Character string. IP address or hostname to trace
#' @param max_hops Integer. Maximum number of hops (default: 30)
#' @param timeout Numeric. Timeout in seconds for each probe (default: 2)
#' @param protocol Character string. Protocol to use ("icmp", "udp", "tcp")
#' @return A list containing traceroute output and metadata
#' @export
#' @examples
#' \dontrun{
#' result <- run_traceroute("8.8.8.8")
#' cat(result$output)
#' }
run_traceroute <- function(target,
                          max_hops = 30,
                          timeout = 2,
                          protocol = c("icmp", "udp", "tcp")) {
  protocol <- match.arg(protocol)

  tryCatch({
    message(sprintf("Running traceroute to %s...", target))

    # Determine the traceroute command based on OS
    os <- tolower(Sys.info()["sysname"])

    if (os == "windows") {
      # Windows tracert command
      cmd <- "tracert"
      args <- c("-h", as.character(max_hops), "-w", as.character(timeout * 1000), target)
    } else if (os == "linux" || os == "darwin") {
      # Unix-like systems
      cmd <- "traceroute"

      # Protocol-specific options
      proto_flag <- switch(protocol,
                          "icmp" = "-I",
                          "udp" = "",
                          "tcp" = "-T")

      args <- c("-m", as.character(max_hops),
               "-w", as.character(timeout),
               proto_flag,
               target)
    } else {
      stop("Unsupported operating system: ", os)
    }

    # Run the traceroute command
    result <- processx::run(cmd, args, error_on_status = FALSE, timeout = 60)

    if (result$status != 0) {
      warning("Traceroute command returned non-zero status: ", result$status)
    }

    output <- list(
      target = target,
      command = paste(c(cmd, args), collapse = " "),
      output = result$stdout,
      error = result$stderr,
      status = result$status,
      timestamp = Sys.time(),
      protocol = protocol,
      max_hops = max_hops,
      timeout = timeout
    )

    message("Traceroute completed.")
    return(output)

  }, error = function(e) {
    stop("Error running traceroute: ", e$message)
  })
}

#' Parse Traceroute Output
#'
#' Parses raw traceroute output into structured data with hop information.
#'
#' @param traceroute_output Character string or list. Raw traceroute output
#' @param source_os Character string. OS that generated the output ("windows", "linux", "darwin")
#' @return A data.frame with hop-by-hop routing information
#' @export
#' @examples
#' \dontrun{
#' result <- run_traceroute("8.8.8.8")
#' parsed <- parse_traceroute_output(result)
#' head(parsed)
#' }
parse_traceroute_output <- function(traceroute_output, source_os = NULL) {
  tryCatch({
    # Extract the actual output text
    if (is.list(traceroute_output)) {
      output_text <- traceroute_output$output
      if (is.null(source_os)) {
        source_os <- tolower(Sys.info()["sysname"])
      }
    } else {
      output_text <- traceroute_output
      if (is.null(source_os)) {
        source_os <- tolower(Sys.info()["sysname"])
      }
    }

    message("Parsing traceroute output...")

    # Split output into lines
    lines <- stringr::str_split(output_text, "\n", simplify = TRUE) %>%
      stringr::str_trim() %>%
      .[. != ""]  # Remove empty lines

    if (length(lines) == 0) {
      warning("No traceroute output to parse")
      return(data.frame())
    }

    # Parse based on OS format
    if (source_os == "windows") {
      parsed_data <- parse_windows_traceroute(lines)
    } else {
      parsed_data <- parse_unix_traceroute(lines)
    }

    # Add metadata
    if (is.list(traceroute_output)) {
      parsed_data <- parsed_data %>%
        dplyr::mutate(
          target = traceroute_output$target,
          protocol = traceroute_output$protocol,
          timestamp = traceroute_output$timestamp
        )
    }

    message(sprintf("Successfully parsed %d hops", nrow(parsed_data)))
    return(parsed_data)

  }, error = function(e) {
    stop("Error parsing traceroute output: ", e$message)
  })
}

#' Parse Windows Tracert Output
#'
#' Internal function to parse Windows tracert output format.
#'
#' @param lines Character vector. Lines from tracert output
#' @return A data.frame with parsed hop data
parse_windows_traceroute <- function(lines) {
  # Skip header lines
  data_lines <- lines[!stringr::str_detect(lines, "^(Tracing route|Maximum hops|traceroute to)")]

  hop_data <- purrr::map_dfr(seq_along(data_lines), function(i) {
    line <- data_lines[i]

    # Windows format: "  1    <1 ms    <1 ms    <1 ms  192.168.1.1"
    # Extract hop number and timing/IP info
    hop_match <- stringr::str_match(line, "^\\s*(\\d+)\\s+(.+)$")

    if (is.na(hop_match[1])) return(NULL)

    hop_number <- as.numeric(hop_match[2])
    rest <- hop_match[3]

    # Extract timing information and IP/hostname
    # Pattern: "time1 time2 time3 ip_or_host"
    timing_ip_match <- stringr::str_match(rest, "^([^\\d]+)?\\s*(\\d+\\s+ms|\\*)\\s+(\\d+\\s+ms|\\*)\\s+(\\d+\\s+ms|\\*)\\s+(.+)$")

    if (!is.na(timing_ip_match[1])) {
      time1 <- timing_ip_match[3]
      time2 <- timing_ip_match[4]
      time3 <- timing_ip_match[5]
      ip_hostname <- stringr::str_trim(timing_ip_match[6])
    } else {
      # Alternative pattern for cases without clear timing
      time1 <- time2 <- time3 <- NA
      ip_hostname <- stringr::str_trim(rest)
    }

    # Clean timing values
    clean_time <- function(time_str) {
      if (is.na(time_str) || time_str == "*") return(NA_real_)
      as.numeric(stringr::str_extract(time_str, "\\d+"))
    }

    data.frame(
      hop = hop_number,
      ip_hostname = ip_hostname,
      rtt1 = clean_time(time1),
      rtt2 = clean_time(time2),
      rtt3 = clean_time(time3),
      avg_rtt = mean(c(clean_time(time1), clean_time(time2), clean_time(time3)), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  return(hop_data)
}

#' Parse Unix Traceroute Output
#'
#' Internal function to parse Unix traceroute output format.
#'
#' @param lines Character vector. Lines from traceroute output
#' @return A data.frame with parsed hop data
parse_unix_traceroute <- function(lines) {
  # Skip header lines
  data_lines <- lines[!stringr::str_detect(lines, "^traceroute to")]

  hop_data <- purrr::map_dfr(seq_along(data_lines), function(i) {
    line <- data_lines[i]

    # Unix format: " 1  router.local (192.168.1.1)  1.234 ms  1.456 ms  2.345 ms"
    # Extract hop number
    hop_match <- stringr::str_match(line, "^\\s*(\\d+)\\s+(.+)$")

    if (is.na(hop_match[1])) return(NULL)

    hop_number <- as.numeric(hop_match[2])
    rest <- hop_match[3]

    # Extract hostname/IP and timing
    # Pattern: "hostname (ip) time1 time2 time3 ..."
    host_ip_match <- stringr::str_match(rest, "^([^\\(]+)?\\s*\\(?([\\d\\.]+)\\)?\\s*(.+)$")

    if (!is.na(host_ip_match[1])) {
      hostname <- stringr::str_trim(host_ip_match[2])
      ip <- host_ip_match[3]
      timing_part <- host_ip_match[4]
    } else {
      hostname <- NA
      ip <- stringr::str_extract(rest, "[\\d\\.]+")
      timing_part <- stringr::str_remove(rest, "[\\d\\.]+")
    }

    # Extract timing values
    timing_matches <- stringr::str_match_all(timing_part, "([\\d\\.]+)\\s*ms")[[1]]
    rtt_values <- as.numeric(timing_matches[,2])

    data.frame(
      hop = hop_number,
      hostname = hostname,
      ip_hostname = ip,
      rtt1 = if(length(rtt_values) >= 1) rtt_values[1] else NA,
      rtt2 = if(length(rtt_values) >= 2) rtt_values[2] else NA,
      rtt3 = if(length(rtt_values) >= 3) rtt_values[3] else NA,
      avg_rtt = if(length(rtt_values) > 0) mean(rtt_values, na.rm = TRUE) else NA,
      stringsAsFactors = FALSE
    )
  })

  return(hop_data)
}

#' Extract AS Path from Traceroute Data
#'
#' Extracts Autonomous System (AS) path information from parsed traceroute data.
#' Requires IP-to-ASN mapping data.
#'
#' @param traceroute_data Data.frame. Parsed traceroute data from parse_traceroute_output
#' @param ip_asn_data Data.frame. IP-to-ASN mapping data
#' @return A data.frame with AS path information added
#' @export
#' @examples
#' \dontrun{
#' traceroute_result <- run_traceroute("8.8.8.8")
#' parsed <- parse_traceroute_output(traceroute_result)
#' asn_data <- collect_dbip_data()
#' as_path <- extract_as_path(parsed, asn_data)
#' }
extract_as_path <- function(traceroute_data, ip_asn_data) {
  if (nrow(traceroute_data) == 0) {
    warning("No traceroute data to process")
    return(data.frame())
  }

  if (nrow(ip_asn_data) == 0) {
    warning("No IP-ASN mapping data provided")
    return(traceroute_data)
  }

  tryCatch({
    message("Extracting AS path from traceroute data...")

    # Function to find ASN for an IP
    find_asn <- function(ip) {
      if (is.na(ip) || ip == "") return(NA)

      # Convert IP to numeric for range lookup
      ip_numeric <- ip_to_numeric(ip)

      # Find matching IP range
      match <- ip_asn_data %>%
        dplyr::filter(start_ip_numeric <= ip_numeric,
                     end_ip_numeric >= ip_numeric) %>%
        dplyr::arrange(dplyr::desc(end_ip_numeric - start_ip_numeric)) %>%  # Prefer more specific ranges
        dplyr::slice(1)

      if (nrow(match) > 0) {
        return(match$asn[1])
      } else {
        return(NA)
      }
    }

    # Add ASN information to traceroute data
    enriched_data <- traceroute_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(asn = find_asn(ip_hostname)) %>%
      dplyr::ungroup()

    # Extract AS path (unique ASNs in order)
    as_path <- enriched_data %>%
      dplyr::filter(!is.na(asn)) %>%
      dplyr::distinct(asn) %>%
      dplyr::pull(asn)

    # Add AS path summary
    enriched_data <- enriched_data %>%
      dplyr::mutate(
        as_path = paste(as_path, collapse = " -> "),
        as_path_length = length(unique(na.omit(enriched_data$asn)))
      )

    message(sprintf("Extracted AS path with %d unique ASNs", length(unique(na.omit(enriched_data$asn)))))
    return(enriched_data)

  }, error = function(e) {
    stop("Error extracting AS path: ", e$message)
  })
}

#' Batch Traceroute Execution
#'
#' Runs traceroute to multiple targets and collects results.
#'
#' @param targets Character vector. List of IP addresses or hostnames
#' @param max_hops Integer. Maximum hops for each traceroute
#' @param timeout Numeric. Timeout for each traceroute
#' @param delay Numeric. Delay between traceroutes in seconds
#' @param progress Logical. Show progress bar
#' @return A list of traceroute results
#' @export
#' @examples
#' \dontrun{
#' targets <- c("8.8.8.8", "1.1.1.1", "208.67.222.222")
#' results <- batch_traceroute(targets, delay = 1)
#' }
batch_traceroute <- function(targets,
                           max_hops = 30,
                           timeout = 2,
                           delay = 0.5,
                           progress = TRUE) {
  if (length(targets) == 0) {
    stop("No targets provided")
  }

  tryCatch({
    message(sprintf("Running batch traceroute for %d targets...", length(targets)))

    results <- list()

    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(targets), style = 3)
    }

    for (i in seq_along(targets)) {
      target <- targets[i]

      message(sprintf("Tracing route to %s (%d/%d)", target, i, length(targets)))

      result <- run_traceroute(target, max_hops = max_hops, timeout = timeout)

      results[[target]] <- result

      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }

      # Delay between requests to avoid overwhelming the network
      if (i < length(targets) && delay > 0) {
        Sys.sleep(delay)
      }
    }

    if (progress) {
      close(pb)
    }

    message("Batch traceroute completed.")
    return(results)

  }, error = function(e) {
    stop("Error in batch traceroute: ", e$message)
  })
}

# Helper function to convert IP to numeric (duplicated from data_collection.R)
ip_to_numeric <- function(ip) {
  if (is.na(ip) || ip == "") return(NA)
  parts <- as.numeric(stringr::str_split(ip, "\\.", simplify = TRUE))
  if (length(parts) != 4) return(NA)
  return(parts[1] * 256^3 + parts[2] * 256^2 + parts[3] * 256 + parts[4])
}

# Parse Windows tracert output
parse_windows_traceroute_output <- function(output_text) {
  if (is.null(output_text) || output_text == "") {
    return(data.frame())
  }

  # Split into lines
  lines <- stringr::str_split(output_text, "\n", simplify = TRUE) %>%
    stringr::str_trim() %>%
    .[. != ""]

  # Remove header lines
  data_lines <- lines[!stringr::str_detect(lines, "^(Tracing route|Maximum hops|traceroute to)")]

  hop_data <- purrr::map_dfr(seq_along(data_lines), function(i) {
    line <- data_lines[i]

    # Windows format: "  1    <1 ms    <1 ms    <1 ms  192.168.1.1"
    # Extract hop number and timing/IP info
    hop_match <- stringr::str_match(line, "^\\s*(\\d+)\\s+(.+)$")

    if (is.na(hop_match[1])) return(NULL)

    hop_number <- as.numeric(hop_match[2])
    rest <- hop_match[3]

    # Extract timing information and IP/hostname
    # Pattern: "time1 time2 time3 ip_or_host"
    timing_ip_match <- stringr::str_match(rest, "^([^\\d]+)?\\s*(\\d+\\s+ms|\\*)\\s+(\\d+\\s+ms|\\*)\\s+(\\d+\\s+ms|\\*)\\s+(.+)$")

    if (!is.na(timing_ip_match[1])) {
      time1 <- timing_ip_match[3]
      time2 <- timing_ip_match[4]
      time3 <- timing_ip_match[5]
      ip_hostname <- stringr::str_trim(timing_ip_match[6])
    } else {
      # Alternative pattern for cases without clear timing
      time1 <- time2 <- time3 <- NA
      ip_hostname <- stringr::str_trim(rest)
    }

    # Clean timing values
    clean_time <- function(time_str) {
      if (is.na(time_str) || time_str == "*") return(NA_real_)
      as.numeric(stringr::str_extract(time_str, "\\d+"))
    }

    data.frame(
      hop = hop_number,
      ip_hostname = ip_hostname,
      rtt1 = clean_time(time1),
      rtt2 = clean_time(time2),
      rtt3 = clean_time(time3),
      avg_rtt = mean(c(clean_time(time1), clean_time(time2), clean_time(time3)), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  return(hop_data)
}

# Parse Unix traceroute output
parse_unix_traceroute_output <- function(output_text) {
  if (is.null(output_text) || output_text == "") {
    return(data.frame())
  }

  # Split into lines
  lines <- stringr::str_split(output_text, "\n", simplify = TRUE) %>%
    stringr::str_trim() %>%
    .[. != ""]

  # Remove header lines
  data_lines <- lines[!stringr::str_detect(lines, "^traceroute to")]

  hop_data <- purrr::map_dfr(seq_along(data_lines), function(i) {
    line <- data_lines[i]

    # Unix format: " 1  router.local (192.168.1.1)  1.234 ms  1.456 ms  2.345 ms"
    # Extract hop number
    hop_match <- stringr::str_match(line, "^\\s*(\\d+)\\s+(.+)$")

    if (is.na(hop_match[1])) return(NULL)

    hop_number <- as.numeric(hop_match[2])
    rest <- hop_match[3]

    # Extract hostname/IP and timing
    # Pattern: "hostname (ip) time1 time2 time3 ..."
    host_ip_match <- stringr::str_match(rest, "^([^\\(]+)?\\s*\\(?([\\d\\.]+)\\)?\\s*(.+)$")

    if (!is.na(host_ip_match[1])) {
      hostname <- stringr::str_trim(host_ip_match[2])
      ip <- host_ip_match[3]
      timing_part <- host_ip_match[4]
    } else {
      hostname <- NA
      ip <- stringr::str_extract(rest, "[\\d\\.]+")
      timing_part <- stringr::str_remove(rest, "[\\d\\.]+")
    }

    # Extract timing values
    timing_matches <- stringr::str_match_all(timing_part, "([\\d\\.]+)\\s*ms")[[1]]
    rtt_values <- as.numeric(timing_matches[,2])

    data.frame(
      hop = hop_number,
      hostname = hostname,
      ip_hostname = ip,
      rtt1 = if(length(rtt_values) >= 1) rtt_values[1] else NA,
      rtt2 = if(length(rtt_values) >= 2) rtt_values[2] else NA,
      rtt3 = if(length(rtt_values) >= 3) rtt_values[3] else NA,
      avg_rtt = if(length(rtt_values) > 0) mean(rtt_values, na.rm = TRUE) else NA,
      stringsAsFactors = FALSE
    )
  })

  return(hop_data)
}
