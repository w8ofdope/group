#!/usr/bin/env Rscript
# Internet Structure Data Collection Worker
# Background service for collecting and updating network data

# Load required packages
library(internetstructure)
library(DBI)
library(RPostgres)
library(redux)
library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)

# Configuration from environment variables
db_config <- list(
  host = Sys.getenv("DB_HOST", "localhost"),
  port = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname = Sys.getenv("DB_NAME", "internet_structure"),
  user = Sys.getenv("DB_USER", "internet_user"),
  password = Sys.getenv("DB_PASSWORD", "internet_pass_2024")
)

redis_config <- list(
  host = Sys.getenv("REDIS_HOST", "localhost"),
  port = as.integer(Sys.getenv("REDIS_PORT", "6379"))
)

# Global variables
worker_id <- paste0("worker_", Sys.getpid())
log_file <- file.path("/app/logs", paste0("worker_", format(Sys.time(), "%Y%m%d"), ".log"))

#' Log message to file and console
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s [%s] %s", timestamp, worker_id, level, message)

  # Write to console
  cat(log_entry, "\n")

  # Write to log file
  tryCatch({
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    cat(sprintf("Failed to write to log file: %s\n", e$message))
  })
}

#' Connect to PostgreSQL database
connect_db <- function() {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      host = db_config$host,
      port = db_config$port,
      dbname = db_config$dbname,
      user = db_config$user,
      password = db_config$password
    )

    # Set search path
    dbExecute(con, "SET search_path TO internet_structure, public")

    log_message("Connected to database")
    return(con)

  }, error = function(e) {
    log_message(sprintf("Failed to connect to database: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Connect to Redis
connect_redis <- function() {
  tryCatch({
    r <- redux::redis(host = redis_config$host, port = redis_config$port)
    log_message("Connected to Redis")
    return(r)
  }, error = function(e) {
    log_message(sprintf("Failed to connect to Redis: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Log data collection operation
log_operation <- function(con, operation_type, data_source, records_processed = 0,
                         records_inserted = 0, records_updated = 0, records_failed = 0,
                         execution_time = NULL, status = "running", error_message = NULL) {
  tryCatch({
    if (is.null(con)) return(NULL)

    dbExecute(con, "
      INSERT INTO data_collection_logs
      (operation_type, data_source, records_processed, records_inserted,
       records_updated, records_failed, execution_time, status, error_message, completed_at)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, CASE WHEN $8 = 'completed' THEN CURRENT_TIMESTAMP ELSE NULL END)
    ", list(operation_type, data_source, records_processed, records_inserted,
            records_updated, records_failed, execution_time, status, error_message))

    log_id <- dbGetQuery(con, "SELECT lastval() as id")$id
    return(log_id)

  }, error = function(e) {
    log_message(sprintf("Failed to log operation: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Update operation log
update_operation_log <- function(con, log_id, records_processed = NULL,
                               records_inserted = NULL, records_updated = NULL,
                               records_failed = NULL, execution_time = NULL,
                               status = NULL, error_message = NULL) {
  tryCatch({
    if (is.null(con) || is.null(log_id)) return()

    updates <- list()
    params <- list()
    param_count <- 0

    if (!is.null(records_processed)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("records_processed = $%d", param_count))
      params <- c(params, records_processed)
    }

    if (!is.null(records_inserted)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("records_inserted = $%d", param_count))
      params <- c(params, records_inserted)
    }

    if (!is.null(records_updated)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("records_updated = $%d", param_count))
      params <- c(params, records_updated)
    }

    if (!is.null(records_failed)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("records_failed = $%d", param_count))
      params <- c(params, records_failed)
    }

    if (!is.null(execution_time)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("execution_time = $%d", param_count))
      params <- c(params, execution_time)
    }

    if (!is.null(status)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("status = $%d", param_count))
      params <- c(params, status)

      if (status == "completed") {
        updates <- c(updates, "completed_at = CURRENT_TIMESTAMP")
      }
    }

    if (!is.null(error_message)) {
      param_count <- param_count + 1
      updates <- c(updates, sprintf("error_message = $%d", param_count))
      params <- c(params, error_message)
    }

    if (length(updates) > 0) {
      query <- sprintf("UPDATE data_collection_logs SET %s WHERE id = $%d",
                      paste(updates, collapse = ", "), param_count + 1)
      params <- c(params, log_id)

      dbExecute(con, query, params)
    }

  }, error = function(e) {
    log_message(sprintf("Failed to update operation log: %s", e$message), "ERROR")
  })
}

#' Collect and store DB-IP data
collect_dbip_data <- function(con) {
  start_time <- Sys.time()
  log_message("Starting DB-IP data collection")

  log_id <- log_operation(con, "collect", "dbip")

  tryCatch({
    # Collect data
    dbip_data <- collect_dbip_data()
    records_processed <- nrow(dbip_data)

    if (records_processed == 0) {
      update_operation_log(con, log_id, status = "failed",
                          error_message = "No data collected from DB-IP")
      return()
    }

    # Insert data into database
    records_inserted <- 0
    records_updated <- 0

    for (i in 1:nrow(dbip_data)) {
      row <- dbip_data[i, ]

      tryCatch({
        # Try to insert, update if conflict
        result <- dbExecute(con, "
          INSERT INTO ip_asn_mappings (start_ip, end_ip, asn, organization, data_source)
          VALUES ($1::inet, $2::inet, $3, $4, $5)
          ON CONFLICT (start_ip, end_ip, asn) DO UPDATE SET
            organization = EXCLUDED.organization,
            updated_at = CURRENT_TIMESTAMP
        ", list(row$start_ip, row$end_ip, row$asn, row$organization, "dbip"))

        if (result > 0) {
          records_inserted <- records_inserted + 1
        }

      }, error = function(e) {
        # Skip this record and continue
        next
      })
    }

    execution_time <- as.interval(start_time, Sys.time())

    update_operation_log(con, log_id,
                        records_processed = records_processed,
                        records_inserted = records_inserted,
                        records_updated = records_updated,
                        execution_time = execution_time,
                        status = "completed")

    log_message(sprintf("DB-IP data collection completed: %d processed, %d inserted",
                       records_processed, records_inserted))

  }, error = function(e) {
    execution_time <- as.interval(start_time, Sys.time())
    update_operation_log(con, log_id,
                        execution_time = execution_time,
                        status = "failed",
                        error_message = e$message)
    log_message(sprintf("DB-IP data collection failed: %s", e$message), "ERROR")
  })
}

#' Collect and store MaxMind geographic data
collect_maxmind_geo_data <- function(con) {
  start_time <- Sys.time()
  log_message("Starting MaxMind geographic data collection")

  log_id <- log_operation(con, "collect", "maxmind_geo")

  tryCatch({
    # Collect data
    geo_data <- collect_maxmind_data()
    records_processed <- nrow(geo_data)

    if (records_processed == 0) {
      update_operation_log(con, log_id, status = "failed",
                          error_message = "No geographic data collected from MaxMind")
      return()
    }

    # Insert data into database
    records_inserted <- 0

    for (i in 1:nrow(geo_data)) {
      row <- geo_data[i, ]

      tryCatch({
        # Construct CIDR notation
        cidr <- paste0(row$start_ip, "/", row$prefix_length)

        result <- dbExecute(con, "
          INSERT INTO geographic_data (network, country_code, country_name,
                                     continent_code, continent_name, data_source)
          VALUES ($1::cidr, $2, $3, $4, $5, $6)
          ON CONFLICT (network) DO UPDATE SET
            country_code = EXCLUDED.country_code,
            country_name = EXCLUDED.country_name,
            continent_code = EXCLUDED.continent_code,
            continent_name = EXCLUDED.continent_name,
            updated_at = CURRENT_TIMESTAMP
        ", list(cidr, row$country_code, row$country_name,
                row$continent_code, row$continent_name, "maxmind"))

        if (result > 0) {
          records_inserted <- records_inserted + 1
        }

      }, error = function(e) {
        next
      })
    }

    execution_time <- as.interval(start_time, Sys.time())

    update_operation_log(con, log_id,
                        records_processed = records_processed,
                        records_inserted = records_inserted,
                        execution_time = execution_time,
                        status = "completed")

    log_message(sprintf("MaxMind geographic data collection completed: %d processed, %d inserted",
                       records_processed, records_inserted))

  }, error = function(e) {
    execution_time <- as.interval(start_time, Sys.time())
    update_operation_log(con, log_id,
                        execution_time = execution_time,
                        status = "failed",
                        error_message = e$message)
    log_message(sprintf("MaxMind geographic data collection failed: %s", e$message), "ERROR")
  })
}

#' Perform batch traceroute operations
perform_batch_traceroute <- function(con, targets = NULL) {
  start_time <- Sys.time()

  # Default targets for monitoring
  if (is.null(targets)) {
    targets <- c("8.8.8.8", "1.1.1.1", "208.67.222.222", "8.8.4.4",
                 "208.67.220.220", "2001:4860:4860::8888", "2001:4860:4860::8844")
  }

  log_message(sprintf("Starting batch traceroute for %d targets", length(targets)))

  log_id <- log_operation(con, "traceroute", "batch")

  tryCatch({
    # Perform traceroutes
    traceroute_results <- batch_traceroute(targets, delay = 1)

    total_traces <- 0
    total_hops <- 0

    for (target in names(traceroute_results)) {
      trace_data <- traceroute_results[[target]]

      if (!is.null(trace_data) && !is.null(trace_data$output)) {
        # Parse traceroute output
        parsed <- parse_traceroute_output(trace_data)

        if (nrow(parsed) > 0) {
          # Get IP-ASN data for ASN lookup
          ip_asn_data <- dbGetQuery(con, "
            SELECT start_ip, end_ip, asn, organization,
                   ('0.0.0.0'::inet + start_ip_numeric)::text as start_ip,
                   ('0.0.0.0'::inet + end_ip_numeric)::text as end_ip,
                   start_ip_numeric, end_ip_numeric
            FROM ip_asn_mappings
            LIMIT 10000
          ")

          # Convert to data frame for ASN lookup
          ip_asn_df <- as.data.frame(ip_asn_data)

          # Extract AS path
          enriched_data <- extract_as_path(parsed, ip_asn_df)

          # Store traceroute result
          trace_result <- dbExecute(con, "
            INSERT INTO traceroute_results (target_ip, target_hostname, protocol, max_hops, total_hops, status)
            VALUES ($1::inet, $2, $3, $4, $5, $6)
          ", list(target, target, trace_data$protocol, trace_data$max_hops,
                  max(enriched_data$hop, na.rm = TRUE), "completed"))

          trace_id <- dbGetQuery(con, "SELECT lastval() as id")$id

          # Store individual hops
          for (j in 1:nrow(enriched_data)) {
            hop <- enriched_data[j, ]

            dbExecute(con, "
              INSERT INTO traceroute_hops (traceroute_id, hop_number, ip_address, hostname, asn, rtt1, rtt2, rtt3, avg_rtt)
              VALUES ($1, $2, $3::inet, $4, $5, $6, $7, $8, $9)
            ", list(trace_id, hop$hop, hop$ip_hostname, hop$hostname, hop$asn,
                    hop$rtt1, hop$rtt2, hop$rtt3, hop$avg_rtt))
          }

          total_traces <- total_traces + 1
          total_hops <- total_hops + nrow(enriched_data)
        }
      }
    }

    execution_time <- as.interval(start_time, Sys.time())

    update_operation_log(con, log_id,
                        records_processed = length(targets),
                        records_inserted = total_traces,
                        execution_time = execution_time,
                        status = "completed")

    log_message(sprintf("Batch traceroute completed: %d traces, %d total hops",
                       total_traces, total_hops))

  }, error = function(e) {
    execution_time <- as.interval(start_time, Sys.time())
    update_operation_log(con, log_id,
                        execution_time = execution_time,
                        status = "failed",
                        error_message = e$message)
    log_message(sprintf("Batch traceroute failed: %s", e$message), "ERROR")
  })
}

#' Calculate and store connectivity metrics
calculate_metrics <- function(con) {
  start_time <- Sys.time()
  log_message("Starting connectivity metrics calculation")

  log_id <- log_operation(con, "metrics", "calculated")

  tryCatch({
    # Get AS data
    as_data <- dbGetQuery(con, "
      SELECT asn, organization, start_ip, end_ip, country_code, country_name,
             ('0.0.0.0'::inet + start_ip_numeric)::text as start_ip,
             ('0.0.0.0'::inet + end_ip_numeric)::text as end_ip,
             start_ip_numeric, end_ip_numeric
      FROM ip_asn_mappings
      LIMIT 50000
    ")

    as_df <- as.data.frame(as_data)

    if (nrow(as_df) == 0) {
      update_operation_log(con, log_id, status = "failed",
                          error_message = "No AS data available for metrics calculation")
      return()
    }

    # Get recent traceroute data
    trace_data <- dbGetQuery(con, "
      SELECT tr.*, th.hop_number, th.ip_address, th.asn as hop_asn, th.avg_rtt
      FROM traceroute_results tr
      JOIN traceroute_hops th ON tr.id = th.traceroute_id
      WHERE tr.timestamp > CURRENT_TIMESTAMP - INTERVAL '24 hours'
    ")

    trace_list <- if (nrow(trace_data) > 0) {
      # Convert to list format expected by calculate_connectivity_metrics
      split(trace_data, trace_data$id)
    } else {
      NULL
    }

    # Calculate metrics
    metrics <- calculate_connectivity_metrics(as_df, trace_list)

    # Store metrics
    for (metric_name in names(metrics)) {
      metric_value <- metrics[[metric_name]]

      # Convert to JSON for storage
      if (is.list(metric_value) || is.data.frame(metric_value)) {
        json_value <- toJSON(metric_value, auto_unbox = TRUE)
      } else {
        json_value <- toJSON(list(value = metric_value), auto_unbox = TRUE)
      }

      dbExecute(con, "
        INSERT INTO connectivity_metrics (metric_name, metric_value, data_source)
        VALUES ($1, $2::jsonb, $3)
        ON CONFLICT (metric_name, calculation_date, data_source) DO UPDATE SET
          metric_value = EXCLUDED.metric_value
      ", list(metric_name, json_value, "worker_calculation"))
    }

    execution_time <- as.interval(start_time, Sys.time())

    update_operation_log(con, log_id,
                        records_processed = length(metrics),
                        records_inserted = length(metrics),
                        execution_time = execution_time,
                        status = "completed")

    log_message(sprintf("Connectivity metrics calculated and stored: %d metrics",
                       length(metrics)))

  }, error = function(e) {
    execution_time <- as.interval(start_time, Sys.time())
    update_operation_log(con, log_id,
                        execution_time = execution_time,
                        status = "failed",
                        error_message = e$message)
    log_message(sprintf("Metrics calculation failed: %s", e$message), "ERROR")
  })
}

#' Main worker loop
run_worker <- function() {
  log_message("Starting Internet Structure Data Collection Worker")
  log_message(sprintf("Worker ID: %s", worker_id))

  # Initialize connections
  con <- connect_db()
  redis <- connect_redis()

  if (is.null(con)) {
    log_message("Cannot start worker without database connection", "ERROR")
    return(1)
  }

  # Worker loop
  iteration <- 0
  while (TRUE) {
    iteration <- iteration + 1
    log_message(sprintf("Starting worker iteration %d", iteration))

    tryCatch({
      # Collect DB-IP data (daily)
      if (iteration %% (24 * 6) == 1) {  # Every 24 hours (assuming 10-minute intervals)
        collect_dbip_data(con)
      }

      # Collect MaxMind geographic data (weekly)
      if (iteration %% (24 * 6 * 7) == 1) {  # Every 7 days
        collect_maxmind_geo_data(con)
      }

      # Perform batch traceroute (hourly)
      if (iteration %% 6 == 1) {  # Every hour
        perform_batch_traceroute(con)
      }

      # Calculate metrics (daily)
      if (iteration %% (24 * 6) == 2) {  # Every 24 hours, offset from data collection
        calculate_metrics(con)
      }

      log_message("Worker iteration completed successfully")

    }, error = function(e) {
      log_message(sprintf("Error in worker iteration: %s", e$message), "ERROR")
    })

    # Sleep for 10 minutes before next iteration
    log_message("Sleeping for 10 minutes...")
    Sys.sleep(10 * 60)
  }

  # Cleanup (this should never be reached in normal operation)
  if (!is.null(con)) {
    dbDisconnect(con)
  }

  return(0)
}

# Run the worker if this script is executed directly
if (!interactive()) {
  quit(status = run_worker())
}
