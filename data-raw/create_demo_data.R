# Simple script to create demo data for the internetstructure package

# Create demo DB-IP data
demo_dbip_data <- data.frame(
  start_ip = c("8.8.8.0", "8.8.4.0", "1.1.1.0", "1.0.0.0", "208.67.222.0", "208.67.220.0"),
  end_ip = c("8.8.8.255", "8.8.4.255", "1.1.1.255", "1.0.0.255", "208.67.222.255", "208.67.220.255"),
  asn = c(15169, 15169, 13335, 13335, 3356, 3356),
  organization = c("Google LLC", "Google LLC", "Cloudflare, Inc.", "Cloudflare, Inc.",
                   "Level 3 Communications, Inc.", "Level 3 Communications, Inc."),
  start_ip_numeric = c(134744072, 134743044, 16843008, 16777216, 3503329792, 3503329536),
  end_ip_numeric = c(134744327, 134743299, 16843263, 16777471, 3503330047, 3503329791)
)

# Create demo geographic data
demo_geo_data <- data.frame(
  network = c("8.8.8.0/24", "8.8.4.0/24", "1.1.1.0/24", "1.0.0.0/24", "208.67.222.0/24", "208.67.220.0/24"),
  country_code = c("US", "US", "US", "US", "US", "US"),
  country_name = c("United States", "United States", "United States", "United States", "United States", "United States"),
  continent_code = c("NA", "NA", "NA", "NA", "NA", "NA"),
  continent_name = c("North America", "North America", "North America", "North America", "North America", "North America"),
  start_ip = c("8.8.8.0", "8.8.4.0", "1.1.1.0", "1.0.0.0", "208.67.222.0", "208.67.220.0"),
  prefix_length = c(24, 24, 24, 24, 24, 24),
  start_ip_numeric = c(134744072, 134743044, 16843008, 16777216, 3503329792, 3503329536)
)

# Create demo unified data
demo_unified_data <- data.frame(
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

# Create demo traceroute data
demo_traceroute_data <- data.frame(
  hop = 1:6,
  ip_hostname = c("192.168.1.1", "10.0.0.1", "203.0.113.1", "8.8.8.8", "8.8.8.8", "8.8.8.8"),
  rtt1 = c(1.2, 5.4, 23.1, 45.2, 44.8, 45.1),
  rtt2 = c(1.1, 5.6, 22.8, 45.8, 45.2, 44.7),
  rtt3 = c(1.3, 5.2, 23.4, 44.9, 45.1, 45.3),
  avg_rtt = c(1.2, 5.4, 23.1, 45.3, 45.0, 45.0),
  asn = c(NA, NA, 3356, 15169, 15169, 15169),
  target = "8.8.8.8",
  protocol = "icmp",
  timestamp = Sys.time(),
  as_path = "3356 -> 15169",
  as_path_length = 2
)

# Save all demo data
save(demo_dbip_data, file = "data/demo_dbip_data.rda", compress = "gzip")
save(demo_geo_data, file = "data/demo_geo_data.rda", compress = "gzip")
save(demo_unified_data, file = "data/demo_unified_data.rda", compress = "gzip")
save(demo_traceroute_data, file = "data/demo_traceroute_data.rda", compress = "gzip")

message("Demo data created successfully!")
message("Files saved:")
message("- data/demo_dbip_data.rda")
message("- data/demo_geo_data.rda")
message("- data/demo_unified_data.rda")
message("- data/demo_traceroute_data.rda")
