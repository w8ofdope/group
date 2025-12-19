#' Data Visualization Module
#'
#' This module provides functions for creating interactive visualizations
#' of Internet structure data using Shiny web applications and static plots.
#'
#' @name visualization
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @import leaflet
#' @import DT
#' @import igraph
#' @import networkD3
#' @import ggplot2
#' @import dplyr
#' @import magrittr
NULL

#' Create Shiny Web Application
#'
#' Creates a comprehensive Shiny web application for exploring Internet structure data.
#'
#' @param as_data Data.frame. Unified AS data (optional - can be loaded in app)
#' @param traceroute_data List. Traceroute results (optional)
#' @param title Character string. Application title
#' @return A Shiny application object
#' @export
#' @examples
#' \dontrun{
#' app <- create_shiny_app()
#' shiny::runApp(app)
#' }
create_shiny_app <- function(as_data = NULL, traceroute_data = NULL,
                           title = "Internet Structure Explorer") {

  # Define UI
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = title),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Overview", tabName = "overview", icon = shiny::icon("dashboard")),
        shinydashboard::menuItem("AS Analysis", tabName = "as_analysis", icon = shiny::icon("sitemap")),
        shinydashboard::menuItem("Geographic View", tabName = "geographic", icon = shiny::icon("globe")),
        shinydashboard::menuItem("Network Graph", tabName = "network", icon = shiny::icon("project-diagram")),
        shinydashboard::menuItem("Traceroute Explorer", tabName = "traceroute", icon = shiny::icon("route")),
        shinydashboard::menuItem("Data Table", tabName = "data_table", icon = shiny::icon("table"))
      )
    ),

    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        # Overview tab
        shinydashboard::tabItem(tabName = "overview",
          shiny::fluidRow(
            shinydashboard::infoBoxOutput("total_asns"),
            shinydashboard::infoBoxOutput("total_countries"),
            shinydashboard::infoBoxOutput("total_ip_ranges")
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("asn_distribution")),
            shiny::column(6, plotly::plotlyOutput("country_distribution"))
          )
        ),

        # AS Analysis tab
        shinydashboard::tabItem(tabName = "as_analysis",
          shiny::fluidRow(
            shiny::column(3,
              shiny::selectInput("asn_select", "Select ASN:",
                               choices = NULL, multiple = TRUE)
            ),
            shiny::column(9,
              DT::dataTableOutput("as_details")
            )
          ),
          shiny::fluidRow(
            plotly::plotlyOutput("as_ip_ranges")
          )
        ),

        # Geographic View tab
        shinydashboard::tabItem(tabName = "geographic",
          shiny::fluidRow(
            shiny::column(12, leaflet::leafletOutput("world_map", height = "600px"))
          )
        ),

        # Network Graph tab
        shinydashboard::tabItem(tabName = "network",
          shiny::fluidRow(
            shiny::column(3,
              shiny::sliderInput("network_size", "Network Size:",
                               min = 10, max = 100, value = 50),
              shiny::selectInput("layout_type", "Layout:",
                               choices = c("force", "circle", "random", "fruchterman"))
            ),
            shiny::column(9,
              networkD3::simpleNetworkOutput("network_graph", height = "600px")
            )
          )
        ),

        # Traceroute Explorer tab
        shinydashboard::tabItem(tabName = "traceroute",
          shiny::fluidRow(
            shiny::column(12,
              shiny::textInput("trace_target", "Target IP/Host:",
                             value = "8.8.8.8"),
              shiny::actionButton("run_trace", "Run Traceroute"),
              shiny::br(), shiny::br()
            )
          ),
          shiny::fluidRow(
            shiny::column(12,
              shiny::h4("Raw Traceroute Output:"),
              shiny::verbatimTextOutput("trace_raw_output"),
              shiny::br()
            )
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("trace_plot")),
            shiny::column(6, DT::dataTableOutput("trace_table"))
          )
        ),

        # Data Table tab
        shinydashboard::tabItem(tabName = "data_table",
          DT::dataTableOutput("full_data_table")
        )
      )
    )
  )

  # Define server
  server <- function(input, output, session) {

    # Ensure magrittr is loaded for %>% operator
    require(magrittr)

    # Load demo data if no data provided
    if (is.null(as_data)) {
      # Create comprehensive inline demo data (600+ records with diverse ASNs, countries, and IP ranges)
      # Major tech companies and ISPs
      asns <- c(15169, 13335, 3356, 701, 7018, 3549, 1299, 3320, 2914, 3257, 6453, 6830, 6939, 20940, 8220, 12389, 15557, 6762, 3303, 6848,
               3491, 6461, 7922, 9002, 1273, 174, 2860, 3320, 2914, 3257, 12956, 15576, 20773, 30844, 31133, 31500, 32934, 33891, 34164, 34549,
               34659, 34984, 35228, 35470, 35625, 35805, 35819, 35908, 36236, 36351, 36459, 36692, 36925, 37100, 37271, 37468, 37662, 37697, 37889, 38001)

      organizations <- c("Google LLC", "Cloudflare, Inc.", "Level 3 Communications, Inc.", "MCI Communications Services, Inc. d/b/a Verizon Business", "AT&T Services, Inc.",
                        "Level 3 Parent, LLC", "Telia Company AB", "Deutsche Telekom AG", "NTT America, Inc.", "GTT Communications Inc.", "TATA Communications",
                        "Liberty Global Operations B.V.", "Hurricane Electric LLC", "Akamai International B.V.", "COLT Technology Services Group Limited",
                        "Rostelecom", "Societe Francaise du Radiotelephone - SFR", "Seabone Net", "Swisscom (Schweiz) AG", "Telenet BVBA",
                        "PCCW Global, Inc.", "Zayo Bandwidth", "Comcast Cable Communications, LLC", "RETN Limited", "Cable & Wireless Worldwide plc",
                        "Cogent Communications", "Nos Comunicacoes, S.A.", "Deutsche Telekom AG", "NTT America, Inc.", "GTT Communications Inc.",
                        "Telefonica Germany GmbH & Co.OHG", "NTS workspace AG", "Host Europe GmbH", "Liquid Telecommunications Ltd", "PJSC MegaFon",
                        "VimpelCom", "Google LLC", "PJSC Vimpelcom", "PJSC MegaFon", "TalkTalk Communications Limited",
                        "Telecom Italia S.p.A.", "Telia Company AB", "Daisy Communications Ltd", "Interoute Communications Limited", "Kabel Deutschland GmbH",
                        "AlpineDC.com", "M247 Ltd", "Vodafone GmbH", "NetActuate, Inc", "SoftLayer Technologies Inc.",
                        "GitHub, Inc.", "OpenTLD BV", "Simcentric Solutions Limited", "SEACOM Limited", "Workonline Communications (Pty) Ltd",
                        "Packet Exchange Limited", "West263 Internet Services", "Node4 Limited", "Redcentric plc", "Hibernia Networks")

      countries <- c("US", "US", "US", "US", "US", "US", "SE", "DE", "US", "US", "IN", "NL", "US", "NL", "GB", "RU", "FR", "US", "CH", "BE",
                    "HK", "US", "US", "GB", "GB", "US", "PT", "DE", "US", "US", "DE", "CH", "DE", "GB", "RU", "RU", "US", "RU", "RU", "GB",
                    "IT", "SE", "GB", "GB", "DE", "CH", "GB", "DE", "US", "US", "US", "NL", "GB", "ZA", "ZA", "GB", "CN", "GB", "GB", "IE")

      country_names <- c("United States", "United States", "United States", "United States", "United States",
                        "United States", "Sweden", "Germany", "United States", "United States",
                        "India", "Netherlands", "United States", "Netherlands", "United Kingdom",
                        "Russia", "France", "United States", "Switzerland", "Belgium",
                        "Hong Kong", "United States", "United States", "United Kingdom", "United Kingdom",
                        "United States", "Portugal", "Germany", "United States", "United States",
                        "Germany", "Switzerland", "Germany", "United Kingdom", "Russia",
                        "Russia", "United States", "Russia", "Russia", "United Kingdom",
                        "Italy", "Sweden", "United Kingdom", "United Kingdom", "Germany",
                        "Switzerland", "United Kingdom", "Germany", "United States", "United States",
                        "United States", "Netherlands", "United Kingdom", "South Africa", "South Africa",
                        "United Kingdom", "China", "United Kingdom", "United Kingdom", "Ireland")

      continents <- c("NA", "NA", "NA", "NA", "NA", "NA", "EU", "EU", "NA", "NA", "AS", "EU", "NA", "EU", "EU", "EU", "EU", "NA", "EU", "EU",
                     "AS", "NA", "NA", "EU", "EU", "NA", "EU", "EU", "NA", "NA", "EU", "EU", "EU", "EU", "EU", "EU", "NA", "EU", "EU", "EU",
                     "EU", "EU", "EU", "EU", "EU", "EU", "EU", "EU", "NA", "NA", "NA", "EU", "EU", "AF", "AF", "EU", "AS", "EU", "EU", "EU")

      continent_names <- c("North America", "North America", "North America", "North America", "North America",
                          "North America", "Europe", "Europe", "North America", "North America",
                          "Asia", "Europe", "North America", "Europe", "Europe",
                          "Europe", "Europe", "North America", "Europe", "Europe",
                          "Asia", "North America", "North America", "Europe", "Europe",
                          "North America", "Europe", "Europe", "North America", "North America",
                          "Europe", "Europe", "Europe", "Europe", "Europe",
                          "Europe", "North America", "Europe", "Europe", "Europe",
                          "Europe", "Europe", "Europe", "Europe", "Europe",
                          "Europe", "Europe", "Europe", "North America", "North America",
                          "North America", "Europe", "Europe", "Africa", "Africa",
                          "Europe", "Asia", "Europe", "Europe", "Europe")

      # Country coordinates (longitude, latitude) - approximate centers/capitals
      country_coords <- list(
        "US" = c(-95.7129, 37.0902),    # Center of USA
        "SE" = c(18.0686, 59.3293),     # Stockholm
        "DE" = c(10.4515, 51.1657),     # Berlin
        "IN" = c(78.9629, 20.5937),     # New Delhi
        "NL" = c(5.2913, 52.1326),      # Amsterdam
        "GB" = c(-0.1278, 51.5074),     # London
        "RU" = c(37.6173, 55.7558),     # Moscow
        "FR" = c(2.3522, 48.8566),      # Paris
        "CH" = c(7.4474, 46.9481),      # Bern
        "BE" = c(4.3517, 50.8503),      # Brussels
        "HK" = c(114.1694, 22.3193),    # Hong Kong
        "PT" = c(-9.1393, 38.7223),     # Lisbon
        "IT" = c(12.4964, 41.9028),     # Rome
        "ZA" = c(28.0473, -26.2041),    # Johannesburg
        "CN" = c(116.4074, 39.9042),    # Beijing
        "IE" = c(-6.2603, 53.3498)      # Dublin
      )

      # Generate 600+ records with diverse combinations
      set.seed(12345)
      n_records <- 600

      as_data <- data.frame(
        asn = sample(asns, n_records, replace = TRUE),
        organization = "",
        start_ip = "",
        end_ip = "",
        country_code = sample(countries, n_records, replace = TRUE),
        country_name = "",
        continent_code = "",
        continent_name = "",
        ip_count = sample(c(256, 512, 1024, 2048, 4096), n_records, replace = TRUE),
        start_ip_numeric = 0,
        end_ip_numeric = 0,
        latitude = 0,
        longitude = 0
      )

      # Fill organization, country_name, continent info based on ASN/country
      for (i in 1:n_records) {
        asn_idx <- match(as_data$asn[i], asns)
        if (!is.na(asn_idx)) {
          as_data$organization[i] <- organizations[asn_idx]
        }

        country_idx <- match(as_data$country_code[i], countries)
        if (!is.na(country_idx)) {
          as_data$country_name[i] <- country_names[country_idx]
          as_data$continent_code[i] <- continents[country_idx]
          as_data$continent_name[i] <- continent_names[country_idx]
        }

        # Generate random IP ranges
        base_ip <- sample(1000000000:4000000000, 1)
        as_data$start_ip_numeric[i] <- base_ip
        as_data$end_ip_numeric[i] <- base_ip + as_data$ip_count[i] - 1

        # Convert to IP strings
        as_data$start_ip[i] <- numeric_to_ip(base_ip)
        as_data$end_ip[i] <- numeric_to_ip(base_ip + as_data$ip_count[i] - 1)

        # Add coordinates based on country
        coords <- country_coords[[as_data$country_code[i]]]
        if (!is.null(coords)) {
          # Add small random offset for visual distribution
          offset_lat <- runif(1, -2, 2)
          offset_lng <- runif(1, -2, 2)
          as_data$latitude[i] <- coords[2] + offset_lat
          as_data$longitude[i] <- coords[1] + offset_lng
        } else {
          as_data$latitude[i] <- runif(1, -90, 90)
          as_data$longitude[i] <- runif(1, -180, 180)
        }
      }

      # Remove duplicates and ensure data quality
      as_data <- as_data[!duplicated(as_data[, c("asn", "start_ip")]), ]
      as_data <- as_data[as_data$asn > 0 & !is.na(as_data$country_code), ]
    }

    if (is.null(traceroute_data)) {
      # Create inline demo traceroute data
      traceroute_data <- data.frame(
        hop = 1:6,
        ip_hostname = c("192.168.1.1", "10.0.0.1", "203.0.113.1", "8.8.8.8", "8.8.8.8", "8.8.8.8"),
        rtt1 = c(1.2, 5.4, 23.1, 45.2, 44.8, 45.1),
        rtt2 = c(1.1, 5.6, 22.8, 45.8, 45.2, 44.7),
        rtt3 = c(1.3, 5.2, 23.4, 44.9, 45.1, 45.3),
        avg_rtt = c(1.2, 5.4, 23.1, 45.3, 45.0, 45.0),
        asn = c(NA, NA, 3356, 15169, 15169, 15169),
        target = "8.8.8.8",
        protocol = "icmp",
        timestamp = as.POSIXct("2024-01-01 12:00:00"),
        as_path = "3356 -> 15169",
        as_path_length = 2
      )
    }

    # Reactive data loading
    as_data_reactive <- shiny::reactiveVal(as_data)
    traceroute_data_reactive <- shiny::reactiveVal(traceroute_data)
    traceroute_raw_output <- shiny::reactiveVal("")

    # Update ASN choices when data loads
    shiny::observe({
      if (!is.null(as_data_reactive())) {
        asn_choices <- sort(unique(as_data_reactive()$asn))
        shiny::updateSelectInput(session, "asn_select",
                                choices = asn_choices,
                                selected = head(asn_choices, 5))
      }
    })

    # Overview statistics
    output$total_asns <- shinydashboard::renderInfoBox({
      data <- as_data_reactive()
      total <- if (!is.null(data)) length(unique(data$asn)) else 0
      shinydashboard::infoBox("Total ASNs", total, icon = shiny::icon("sitemap"),
                             color = "blue")
    })

    output$total_countries <- shinydashboard::renderInfoBox({
      data <- as_data_reactive()
      total <- if (!is.null(data)) length(unique(na.omit(data$country_code))) else 0
      shinydashboard::infoBox("Countries", total, icon = shiny::icon("globe"),
                             color = "green")
    })

    output$total_ip_ranges <- shinydashboard::renderInfoBox({
      data <- as_data_reactive()
      total <- if (!is.null(data)) nrow(data) else 0
      shinydashboard::infoBox("IP Ranges", total, icon = shiny::icon("network-wired"),
                             color = "purple")
    })

    # ASN distribution plot
    output$asn_distribution <- plotly::renderPlotly({
      data <- as_data_reactive()
      if (is.null(data)) return(NULL)

      # Count IP ranges per ASN (top 20)
      asn_counts <- data %>%
        dplyr::count(asn, organization) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        head(20)

      plotly::plot_ly(asn_counts, x = ~reorder(as.factor(asn), n), y = ~n,
                     type = "bar", name = "IP Ranges") %>%
        plotly::layout(title = "Top 20 ASNs by IP Range Count",
                      xaxis = list(title = "ASN"),
                      yaxis = list(title = "IP Ranges"))
    })

    # Country distribution plot
    output$country_distribution <- plotly::renderPlotly({
      data <- as_data_reactive()
      if (is.null(data)) return(NULL)

      country_counts <- data %>%
        dplyr::filter(!is.na(country_name)) %>%
        dplyr::count(country_name) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        head(15)

      plotly::plot_ly(country_counts, x = ~n, y = ~reorder(country_name, n),
                     type = "bar", orientation = "h", name = "ASNs") %>%
        plotly::layout(title = "Top 15 Countries by ASN Count",
                      xaxis = list(title = "ASN Count"),
                      yaxis = list(title = "Country"))
    })

    # AS details table
    output$as_details <- DT::renderDataTable({
      selected_asns <- input$asn_select
      data <- as_data_reactive()

      if (is.null(data) || is.null(selected_asns)) return(NULL)

      data %>%
        dplyr::filter(asn %in% selected_asns) %>%
        dplyr::select(asn, organization, start_ip, end_ip, country_name,
                     ip_count, data_source) %>%
        DT::datatable(options = list(pageLength = 10))
    })

    # AS IP ranges plot
    output$as_ip_ranges <- plotly::renderPlotly({
      selected_asns <- input$asn_select
      data <- as_data_reactive()

      if (is.null(data) || is.null(selected_asns)) return(NULL)

      plot_data <- data %>%
        dplyr::filter(asn %in% selected_asns) %>%
        dplyr::mutate(asn_label = paste0("AS", asn))

      plotly::plot_ly(plot_data, x = ~start_ip_numeric, y = ~asn_label,
                     type = "scatter", mode = "markers",
                     marker = list(size = 8, opacity = 0.6)) %>%
        plotly::layout(title = "IP Ranges by ASN",
                      xaxis = list(title = "IP Address (numeric)"),
                      yaxis = list(title = "ASN"))
    })

    # World map
    output$world_map <- leaflet::renderLeaflet({
      data <- as_data_reactive()
      if (is.null(data)) return(NULL)

      # Aggregate data by country with coordinates
      country_data <- data %>%
        dplyr::filter(!is.na(country_code), !is.na(country_name), !is.na(latitude), !is.na(longitude)) %>%
        dplyr::group_by(country_code, country_name) %>%
        dplyr::summarise(
          asn_count = length(unique(asn)),
          ip_ranges = dplyr::n(),
          latitude = mean(latitude, na.rm = TRUE),
          longitude = mean(longitude, na.rm = TRUE),
          .groups = "drop"
        )

      # Create leaflet map
      leaflet::leaflet(country_data) %>%
        leaflet::addTiles() %>%
        leaflet::addCircles(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(asn_count) * 50000,
          popup = ~paste0(country_name, "<br>",
                         "ASNs: ", asn_count, "<br>",
                         "IP Ranges: ", ip_ranges),
          color = "blue", fillOpacity = 0.7
        ) %>%
        leaflet::setView(lng = 0, lat = 20, zoom = 2)  # Center on world
    })

    # Network graph (simplified)
    output$network_graph <- networkD3::renderSimpleNetwork({
      size <- input$network_size
      data <- as_data_reactive()

      if (is.null(data)) return(NULL)

      # Create a simple network from top ASNs
      top_asns <- data %>%
        dplyr::count(asn, organization) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        head(size)

      # Create links (simplified - just connect consecutive ASNs)
      links <- data.frame(
        source = head(top_asns$organization, -1),
        target = tail(top_asns$organization, -1)
      )

      networkD3::simpleNetwork(links, height = 600)
    })

    # Real Traceroute functionality with progressive display
    shiny::observeEvent(input$run_trace, {
      target <- input$trace_target

      # Validate target
      if (is.null(target) || target == "") {
        shiny::showNotification("Please enter a valid target IP or hostname", type = "error")
        return()
      }

      # Initialize empty results
      traceroute_data_reactive(data.frame())

      # Show initial progress notification
      shiny::showNotification("Starting traceroute...", type = "message", duration = 2)

      tryCatch({
        # Try to run traceroute using system() which is more reliable
        os <- tolower(Sys.info()["sysname"])

        # Construct command based on OS with faster settings
        if (os == "windows") {
          # Windows tracert command - faster settings
          full_cmd <- sprintf('tracert -h 10 -w 500 "%s"', target)  # Even faster: 10 hops, 500ms timeout
        } else if (os %in% c("linux", "darwin")) {
          # Unix traceroute - faster settings
          full_cmd <- sprintf('traceroute -m 10 -w 0.5 "%s"', target)  # Even faster: 10 hops, 0.5s timeout
        } else {
          stop("Unsupported operating system: ", os)
        }

        # Run the traceroute command using system() with shorter timeout
        message("Executing traceroute from your system IP: ", full_cmd)

        # Use a progress indicator
        progress <- shiny::Progress$new()
        progress$set(message = "Running traceroute...", value = 0.1)

        # Execute command and handle encoding issues
        system_result <- tryCatch({
          system(full_cmd, intern = TRUE, timeout = 10)
        }, error = function(e) {
          # If encoding fails, try with different approach
          message("Standard execution failed, trying alternative approach...")
          system(full_cmd, intern = TRUE, timeout = 10, ignore.stderr = FALSE)
        })

        # Handle encoding issues - convert to UTF-8
        raw_output <- ""
        if (length(system_result) > 0) {
          system_result <- iconv(system_result, from = "", to = "UTF-8", sub = "?")
          raw_output <- paste(system_result, collapse = "\n")

          # Also print to console as requested
          cat("TRACEROUTE OUTPUT:\n")
          cat(system_result, sep = "\n")
          cat("\n")

          # Update raw output for display in app
          traceroute_raw_output(raw_output)
        }

        progress$set(value = 0.5, message = "Parsing results...")

        if (length(system_result) == 0) {
          warning("Traceroute command returned no output")
          progress$close()
          shiny::showNotification("Traceroute returned no output. Using demo data.", type = "warning")

          # Fallback to demo data with progressive display
          demo_data <- data.frame(
            hop = integer(),
            ip_hostname = character(),
            rtt1 = numeric(),
            rtt2 = numeric(),
            rtt3 = numeric(),
            avg_rtt = numeric(),
            stringsAsFactors = FALSE
          )

          # Simulate progressive display of demo data
          for (i in 1:6) {
            Sys.sleep(0.3)  # Small delay for visual effect

            new_row <- data.frame(
              hop = i,
              ip_hostname = c("192.168.1.1", "10.0.0.1", "203.0.113.1", "8.8.8.8", "8.8.8.8", "8.8.8.8")[i],
              rtt1 = c(1.2, 5.4, 23.1, 45.2, 44.8, 45.1)[i],
              rtt2 = c(1.1, 5.6, 22.8, 45.8, 45.2, 44.7)[i],
              rtt3 = c(1.3, 5.2, 23.4, 44.9, 45.1, 45.3)[i],
              avg_rtt = c(1.2, 5.4, 23.1, 45.3, 45.0, 45.0)[i],
              stringsAsFactors = FALSE
            )

            demo_data <- rbind(demo_data, new_row)
            traceroute_data_reactive(demo_data)

            progress$set(value = 0.5 + (i/6)*0.4, message = sprintf("Processing hop %d...", i))
          }

          parsed_data <- demo_data
        } else {
          # Parse the output progressively
          output_text <- paste(system_result, collapse = "\n")

          # Parse based on OS
          if (os == "windows") {
            parsed_data <- parse_windows_traceroute_output(output_text)
          } else {
            parsed_data <- parse_unix_traceroute_output(output_text)
          }

          if (nrow(parsed_data) == 0) {
            warning("Failed to parse traceroute output")
            progress$close()
            shiny::showNotification("Failed to parse traceroute output. Using demo data.", type = "warning")

            # Fallback to demo data
            parsed_data <- data.frame(
              hop = 1:4,
              ip_hostname = c("192.168.1.1", "10.0.0.1", "8.8.8.8", target),
              rtt1 = c(1.2, 5.4, 23.1, 45.2),
              rtt2 = c(1.1, 5.6, 22.8, 45.8),
              rtt3 = c(1.3, 5.2, 23.4, 44.9),
              avg_rtt = c(1.2, 5.4, 23.1, 45.0),
              stringsAsFactors = FALSE
            )
          }

          # Simulate progressive display for real data
          temp_data <- data.frame()
          for (i in 1:nrow(parsed_data)) {
            Sys.sleep(0.2)  # Small delay for visual effect
            temp_data <- rbind(temp_data, parsed_data[i, ])
            traceroute_data_reactive(temp_data)
            progress$set(value = 0.5 + (i/nrow(parsed_data))*0.4,
                        message = sprintf("Processing hop %d of %d...", i, nrow(parsed_data)))
          }
        }

        progress$set(value = 0.9, message = "Adding AS information...")

        # Add AS information if we have demo data
        as_demo_data <- as_data_reactive()
        if (!is.null(as_demo_data) && nrow(as_demo_data) > 0) {
          parsed_data$asn <- sapply(parsed_data$ip_hostname, function(ip) {
            if (is.na(ip) || ip == "" || !validate_ip(ip, "ipv4")) return(NA)
            ip_to_asn(ip, as_demo_data)
          })
        } else {
          parsed_data$asn <- NA
        }

        # Create AS path
        valid_asns <- na.omit(parsed_data$asn)
        if (length(valid_asns) > 0) {
          parsed_data$as_path <- paste(valid_asns, collapse = " -> ")
          parsed_data$as_path_length <- length(valid_asns)
        } else {
          parsed_data$as_path <- ""
          parsed_data$as_path_length <- 0
        }

        progress$set(value = 1.0, message = "Complete!")
        progress$close()

        # Final update of reactive data
        traceroute_data_reactive(parsed_data)

        shiny::showNotification(sprintf("Traceroute completed! Found %d hops", nrow(parsed_data)), type = "message", duration = 3)

      }, error = function(e) {
        if (exists("progress")) progress$close()
        shiny::showNotification(paste("Traceroute error:", e$message), type = "error")
        warning("Traceroute error: ", e$message)
      })
    })

    # Raw traceroute output display
    output$trace_raw_output <- shiny::renderText({
      raw_output <- traceroute_raw_output()
      if (is.null(raw_output) || raw_output == "") {
        "No traceroute output available. Click 'Run Traceroute' to start."
      } else {
        raw_output
      }
    })

    # Full data table
    output$full_data_table <- DT::renderDataTable({
      data <- as_data_reactive()
      if (is.null(data)) return(NULL)

      DT::datatable(data, options = list(pageLength = 25))
    })
  }

  # Return Shiny app
  shiny::shinyApp(ui, server)
}

#' Run Shiny Application
#'
#' Launches the Internet Structure Explorer Shiny application.
#'
#' @param as_data Data.frame. Unified AS data (optional)
#' @param traceroute_data List. Traceroute results (optional)
#' @param port Integer. Port to run the application on (default: 3838)
#' @param host Character string. Host address (default: "127.0.0.1")
#' @export
#' @examples
#' \dontrun{
#' run_shiny_app()
#' }
run_shiny_app <- function(as_data = NULL, traceroute_data = NULL,
                         port = 3838, host = "127.0.0.1") {
  # Demo data is loaded automatically in create_shiny_app if not provided
  app <- create_shiny_app(as_data, traceroute_data)
  shiny::runApp(app, port = port, host = host)
}

#' Create Network Plot
#'
#' Creates a static network visualization of AS relationships.
#'
#' @param as_data Data.frame. Unified AS data
#' @param traceroute_data List. Traceroute results for network edges
#' @param max_nodes Integer. Maximum number of nodes to display
#' @return A ggplot object
#' @export
#' @examples
#' \dontrun{
#' plot <- create_network_plot(as_data)
#' print(plot)
#' }
create_network_plot <- function(as_data, traceroute_data = NULL, max_nodes = 50) {
  if (nrow(as_data) == 0) {
    warning("No AS data available for plotting")
    return(NULL)
  }

  tryCatch({
    # Get top ASNs by IP range count
    top_asns <- as_data %>%
      dplyr::count(asn, organization) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      head(max_nodes)

    # Create nodes data frame
    nodes <- top_asns %>%
      dplyr::mutate(
        id = asn,
        label = paste0("AS", asn),
        size = log(n + 1) * 3
      )

    # Create edges from traceroute data if available
    edges <- data.frame(from = integer(), to = integer())

    if (!is.null(traceroute_data) && length(traceroute_data) > 0) {
      # Extract AS paths from traceroute data
      for (trace_result in traceroute_data) {
        if (!is.null(trace_result) && "as_path" %in% names(trace_result)) {
          as_path <- trace_result$as_path
          if (!is.na(as_path) && as_path != "") {
            asn_list <- as.numeric(stringr::str_split(as_path, " -> ")[[1]])
            asn_list <- asn_list[!is.na(asn_list)]

            if (length(asn_list) > 1) {
              for (i in 1:(length(asn_list) - 1)) {
                edges <- dplyr::bind_rows(edges,
                  data.frame(from = asn_list[i], to = asn_list[i + 1])
                )
              }
            }
          }
        }
      }
    }

    # If no traceroute data, create random edges for visualization
    if (nrow(edges) == 0) {
      set.seed(42)
      for (i in 1:min(20, nrow(nodes))) {
        from_idx <- sample(1:nrow(nodes), 1)
        to_idx <- sample(1:nrow(nodes), 1)
        if (from_idx != to_idx) {
          edges <- dplyr::bind_rows(edges,
            data.frame(from = nodes$id[from_idx], to = nodes$id[to_idx])
          )
        }
      }
    }

    # Create igraph object
    g <- igraph::graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

    # Calculate layout
    layout <- igraph::layout_with_fr(g)

    # Create plot
    plot_data <- data.frame(
      x = layout[, 1],
      y = layout[, 2],
      label = igraph::V(g)$label,
      size = igraph::V(g)$size,
      organization = igraph::V(g)$organization
    )

    # Create edge data for plotting
    edge_data <- data.frame(
      x = numeric(),
      y = numeric(),
      group = integer()
    )

    if (nrow(edges) > 0) {
      for (i in 1:nrow(edges)) {
        from_vertex <- which(igraph::V(g)$name == as.character(edges$from[i]))
        to_vertex <- which(igraph::V(g)$name == as.character(edges$to[i]))

        if (length(from_vertex) > 0 && length(to_vertex) > 0) {
          edge_data <- dplyr::bind_rows(edge_data,
            data.frame(
              x = c(layout[from_vertex, 1], layout[to_vertex, 1]),
              y = c(layout[from_vertex, 2], layout[to_vertex, 2]),
              group = i
            )
          )
        }
      }
    }

    # Create the plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = edge_data,
                           ggplot2::aes(x = x, y = y, group = group),
                           color = "gray70", alpha = 0.5, linewidth = 0.5) +
      ggplot2::geom_point(data = plot_data,
                         ggplot2::aes(x = x, y = y, size = size),
                         color = "blue", alpha = 0.7) +
      ggplot2::geom_text(data = plot_data,
                        ggplot2::aes(x = x, y = y, label = label),
                        size = 3, vjust = -1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "none"
      ) +
      ggplot2::labs(title = "AS Network Graph")

    return(p)

  }, error = function(e) {
    warning("Error creating network plot: ", e$message)
    return(NULL)
  })
}

#' Create Geographic Map
#'
#' Creates a geographic visualization of AS data on a world map.
#'
#' @param as_data Data.frame. Unified AS data with geographic information
#' @param metric Character string. Metric to visualize ("asn_count", "ip_ranges")
#' @return A ggplot object with world map
#' @export
#' @examples
#' \dontrun{
#' map <- create_geographic_map(as_data)
#' print(map)
#' }
create_geographic_map <- function(as_data, metric = c("asn_count", "ip_ranges")) {
  metric <- match.arg(metric)

  if (nrow(as_data) == 0) {
    warning("No AS data available for mapping")
    return(NULL)
  }

  tryCatch({
    # Aggregate data by country
    country_data <- as_data %>%
      dplyr::filter(!is.na(country_code)) %>%
      dplyr::group_by(country_code, country_name) %>%
      dplyr::summarise(
        asn_count = length(unique(asn)),
        ip_ranges = dplyr::n(),
        .groups = "drop"
      )

    # Get world map data
    world <- ggplot2::map_data("world")

    # Merge with country data
    world_data <- world %>%
      dplyr::left_join(country_data, by = c("region" = "country_name"))

    # Create the metric column
    world_data$metric_value <- switch(metric,
      "asn_count" = world_data$asn_count,
      "ip_ranges" = world_data$ip_ranges
    )

    # Create the map
    p <- ggplot2::ggplot(world_data) +
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group,
                                       fill = metric_value),
                           color = "white", linewidth = 0.2) +
      ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue",
                                  na.value = "gray90",
                                  name = switch(metric,
                                               "asn_count" = "ASN Count",
                                               "ip_ranges" = "IP Ranges")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        title = paste("Global Distribution of",
                     switch(metric, "asn_count" = "ASNs", "ip_ranges" = "IP Ranges"))
      )

    return(p)

  }, error = function(e) {
    warning("Error creating geographic map: ", e$message)
    return(NULL)
  })
}
