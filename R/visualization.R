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
      # Create inline demo data instead of loading from .rda files
      as_data <- data.frame(
        asn = c(15169, 15169, 13335, 13335, 3356, 3356),
        organization = c("Google LLC", "Google LLC", "Cloudflare, Inc.", "Cloudflare, Inc.",
                        "Level 3 Communications, Inc.", "Level 3 Communications, Inc."),
        start_ip = c("8.8.8.0", "8.8.4.0", "1.1.1.0", "1.0.0.0", "208.67.222.0", "208.67.220.0"),
        end_ip = c("8.8.8.255", "8.8.4.255", "1.1.1.255", "1.0.0.255", "208.67.222.255", "208.67.220.255"),
        country_code = c("US", "US", "US", "US", "US", "US"),
        country_name = c("United States", "United States", "United States", "United States", "United States", "United States"),
        continent_code = c("NA", "NA", "NA", "NA", "NA", "NA"),
        continent_name = c("North America", "North America", "North America", "North America", "North America", "North America"),
        ip_count = c(256, 256, 256, 256, 256, 256),
        start_ip_numeric = c(134744072, 134743044, 16843008, 16777216, 3503329792, 3503329536),
        end_ip_numeric = c(134744327, 134743299, 16843263, 16777471, 3503330047, 3503329791)
      )
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

      # Aggregate data by country
      country_data <- data %>%
        dplyr::filter(!is.na(country_code), !is.na(country_name)) %>%
        dplyr::group_by(country_code, country_name) %>%
        dplyr::summarise(
          asn_count = length(unique(asn)),
          ip_ranges = dplyr::n(),
          .groups = "drop"
        )

      # Create leaflet map
      leaflet::leaflet(country_data) %>%
        leaflet::addTiles() %>%
        leaflet::addCircles(
          lng = ~0, lat = ~0,  # Placeholder - would need actual coordinates
          radius = ~sqrt(asn_count) * 50000,
          popup = ~paste0(country_name, "<br>",
                         "ASNs: ", asn_count, "<br>",
                         "IP Ranges: ", ip_ranges),
          color = "blue", fillOpacity = 0.7
        )
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

    # Traceroute functionality
    shiny::observeEvent(input$run_trace, {
      target <- input$trace_target

      # Show notification
      shiny::showNotification("Loading demo traceroute data...", type = "message")

      # Use demo traceroute data
      trace_data <- traceroute_data_reactive()

      # Filter by target if possible
      if (!is.null(trace_data) && nrow(trace_data) > 0) {
        # For demo, just show the data
        display_data <- trace_data
      } else {
        # Fallback mock data
        display_data <- data.frame(
          hop = 1:5,
          ip_hostname = c("192.168.1.1", "10.0.0.1", "203.0.113.1", target, target),
          avg_rtt = c(1.2, 5.4, 23.1, 45.2, 44.8),
          asn = c(NA, NA, 3356, 15169, 15169)
        )
      }

      output$trace_table <- DT::renderDataTable({
        DT::datatable(display_data, options = list(pageLength = 10))
      })

      output$trace_plot <- plotly::renderPlotly({
        plotly::plot_ly(display_data, x = ~hop, y = ~avg_rtt, type = "scatter",
                       mode = "lines+markers", name = "RTT") %>%
          plotly::layout(title = paste("Traceroute to", target),
                        xaxis = list(title = "Hop"),
                        yaxis = list(title = "RTT (ms)"))
      })

      shiny::showNotification("Demo traceroute data loaded!", type = "message", duration = 2)
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
