plot_outage_timeseries <- function(data, state_name, start_date, end_date) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  outages <- data %>% 
    filter(state == state_name,
           datetime > dmy(start_date) & datetime < dmy(end_date)) %>%
    mutate(date = as_date(datetime)) %>%
    group_by(date) %>%
    summarise(total_customers_out = max(customers_out, na.rm = TRUE))
  
  ggplot(outages, aes(x = date, y = total_customers_out)) +
    geom_line(color = "red", size = 1.2) +
    geom_point(color = "darkred", size = 2) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Power Outages in", state_name),
      x = "Date",
      y = "Maximum Customers Out"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_outage_map <- function(data, state_name, state_abbr, start_date, end_date) {
  library(dplyr)
  library(tigris)
  library(tmap)
  library(lubridate)
  
  # Calculate max outages per county
  county_outages <- data %>% 
    filter(state == state_name,
           datetime > dmy(start_date) & datetime < dmy(end_date)) %>%
    group_by(fips_code, county) %>%
    summarise(max_customers_out = max(customers_out, na.rm = TRUE), .groups = "drop")
  
  # Get county boundaries
  counties <- counties(state = state_abbr, year = 2019, resolution = "500k", cb = TRUE) %>%
    select(GEOID, NAME)
  
  # Merge and handle NAs
  map_data <- left_join(counties, county_outages, by = c("GEOID" = "fips_code")) %>%
    mutate(max_customers_out = if_else(is.na(max_customers_out), 0, max_customers_out))
  
  # Create map
  tmap_mode("plot")
  tm_shape(map_data) +
    tm_fill("max_customers_out", 
            style = "jenks", n = 7, palette = "YlOrRd",
            title = "Max Customers Out") +
    tm_borders(col = "gray40", lwd = 0.3) +
    tm_layout(
      main.title = paste("Maximum Customers Without Power\n", state_name),
      main.title.position = "center",
      main.title.size = 1.2,
      legend.outside = TRUE,
      frame = FALSE
    )
}



library(readr)
options(tigris_use_cache = TRUE)

eaglei.2021 <- read_csv("eaglei_outages_2021.csv") %>% 
  rename(datetime = run_start_time)

#Texas Winter Storm Uri

plot_outage_timeseries(eaglei.2021, "Texas", "13-2-2021", "23-2-2021")

plot_outage_map(eaglei.2021, "Texas", "TX", "13-2-2021", "23-2-2021")

#Hurricane Ida	Louisiana

plot_outage_timeseries(eaglei.2021, "Louisiana", "27-8-2021", "25-9-2021")

plot_outage_map(eaglei.2021, "Louisiana", "LA", "13-2-2021", "23-2-2021")

#Hurricane Irma	Florida

eaglei.2017 <- read_csv("eaglei_outages_2017.csv") %>% 
  rename(datetime = run_start_time)

plot_outage_timeseries(eaglei.2017, "Florida", "8-9-2017", "20-9-2017")

plot_outage_map(eaglei.2017, "Florida", "FL", "8-9-2017", "20-9-2017")
