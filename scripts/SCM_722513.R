# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidysynth)

# 1. Load Data using Relative Paths
# Assuming this script is executed from the /scripts directory within the repository
data_09_19_722513 <- read.csv("../data/data_09_19_722513.csv", colClasses = c("area_fips" = "character"))

california_fips <- data_09_19_722513 %>%
  filter(treat == 1) %>%
  pull(area_fips)

# 2. Define the core Synthetic Control function to avoid repetition
run_county_scm <- function(target_fips, target_size, df) {
  
  # Filter data for the specific treated county and matching-sized donors
  synth_in <- df %>%
    filter(area_fips == target_fips | 
             (area_fips %in% setdiff(unique(df$area_fips), california_fips) & size == target_size))
  
  # Run the tidysynth pipeline exactly as specified in the original script
  synth_out <- synth_in %>%
    synthetic_control(
      outcome = norm_2014_avg_qtrly_emplvl, 
      unit = area_fips, 
      time = time_fe, 
      i_unit = target_fips, 
      i_time = 23, 
      generate_placebos = FALSE # Set to FALSE as placebos are not plotted in the gap plots
    ) %>%
    generate_predictor(
      time_window = 1:16, 
      norm_2014_avg_qtrly_emplvl_1 = norm_2014_avg_qtrly_emplvl[1],
      norm_2014_avg_qtrly_emplvl_2 = norm_2014_avg_qtrly_emplvl[2],
      norm_2014_avg_qtrly_emplvl_3 = norm_2014_avg_qtrly_emplvl[3],
      norm_2014_avg_qtrly_emplvl_4 = norm_2014_avg_qtrly_emplvl[4],
      norm_2014_avg_qtrly_emplvl_5 = norm_2014_avg_qtrly_emplvl[5],
      norm_2014_avg_qtrly_emplvl_6 = norm_2014_avg_qtrly_emplvl[6],
      norm_2014_avg_qtrly_emplvl_7 = norm_2014_avg_qtrly_emplvl[7],
      norm_2014_avg_qtrly_emplvl_8 = norm_2014_avg_qtrly_emplvl[8],
      norm_2014_avg_qtrly_emplvl_9 = norm_2014_avg_qtrly_emplvl[9],
      norm_2014_avg_qtrly_emplvl_10 = norm_2014_avg_qtrly_emplvl[10],
      norm_2014_avg_qtrly_emplvl_11 = norm_2014_avg_qtrly_emplvl[11],
      norm_2014_avg_qtrly_emplvl_12 = norm_2014_avg_qtrly_emplvl[12],
      norm_2014_avg_qtrly_emplvl_13 = norm_2014_avg_qtrly_emplvl[13],
      norm_2014_avg_qtrly_emplvl_14 = norm_2014_avg_qtrly_emplvl[14],
      norm_2014_avg_qtrly_emplvl_15 = norm_2014_avg_qtrly_emplvl[15],
      norm_2014_avg_qtrly_emplvl_16 = norm_2014_avg_qtrly_emplvl[16],
      avg_norm_2014_qtrly_estabs_count = mean(norm_2014_qtrly_estabs_count, na.rm = TRUE),
      avg_norm_2014_avg_qtrly_emplvl = mean(norm_2014_avg_qtrly_emplvl, na.rm = TRUE),
      avg_norm_2014_total_employed = mean(norm_2014_total_employed, na.rm = TRUE),
      avg_total_unemployment_rate = mean(total_unemployment_rate, na.rm = TRUE)
    ) %>%
    generate_weights(optimization_window = 1:16, margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
    generate_control()
  
  # Extract and return the employment gap
  outcomes <- synth_out %>%
    grab_synthetic_control() %>%
    mutate(gap_percent = real_y - synth_y, area_fips = target_fips) %>%
    select(time_unit, gap_percent, area_fips)
  
  return(outcomes)
}

# 3. Helper function to calculate weighted gaps, plot, and save the images
generate_gap_plot <- function(group_name, df) {
  
  # Filter for the specific group (or all if group_name == "All")
  if(group_name == "All") {
    treated_units <- df %>% filter(treat == 1) %>% pull(area_fips) %>% unique()
    title_text <- "Average Estimated Effect Across California Counties vs Synthetic Controls"
  } else {
    treated_units <- df %>% filter(size == group_name, treat == 1) %>% pull(area_fips) %>% unique()
    title_text <- paste("Average Estimated Effect Across", group_name, "California Counties vs Synthetic Controls")
  }
  
  # Get Population Weights
  pop_data <- df %>%
    filter(area_fips %in% treated_units) %>%
    distinct(area_fips, population_2014) %>%
    mutate(population_weight = population_2014 / sum(population_2014, na.rm = TRUE))
  
  # Run SCM for each county in the selected group
  gaps_list <- lapply(treated_units, function(fips) {
    size_cat <- df %>% filter(area_fips == fips) %>% pull(size) %>% .[1]
    run_county_scm(fips, size_cat, df)
  })
  
  # Combine gaps and apply population weights
  weighted_gaps <- bind_rows(gaps_list) %>%
    left_join(pop_data, by = "area_fips") %>%
    mutate(
      weighted_gap_percent = gap_percent * population_weight,
      relative_time = time_unit - 23
    )
  
  # Calculate average weighted gaps over time
  avg_weighted_gaps <- weighted_gaps %>%
    group_by(relative_time) %>%
    summarize(avg_gap_percent = sum(weighted_gap_percent, na.rm = TRUE))
  
  # Construct the plot exactly as formatted in the original script
  p <- ggplot() +
    geom_point(data = weighted_gaps, aes(x = relative_time, y = gap_percent, size = population_weight), 
               color = "lightblue", alpha = 0.6) +
    geom_line(data = avg_weighted_gaps, aes(x = relative_time, y = avg_gap_percent), color = "blue", size = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    scale_x_continuous(
      breaks = seq(min(weighted_gaps$relative_time), max(weighted_gaps$relative_time), by = 2), 
      expand = c(0, 0)
    ) +
    labs(
      y = "Difference in Quarterly Employment Level (Percent)",
      x = "Time Relative to Minimum Wage Change (quarters)",
      title = title_text,
      subtitle = "Solid blue line represents population-weighted average effect; circles show individual estimated effects"
    ) +
    scale_size_continuous(name = "Relative Population Weight") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 0),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_cartesian(clip = "off")
  
  # Save the final plot securely to the output directory using relative paths
  filename <- paste0("../output/", tolower(group_name), "_counties_norm_emplvl_722513_gap_plot.png")
  ggsave(filename, plot = p, width = 12, height = 6, bg = "white")
  message("Successfully saved: ", filename)
}

# 4. Execute the plotting function for all four required cohorts
generate_gap_plot("Large", data_09_19_722513)
generate_gap_plot("Medium", data_09_19_722513)
generate_gap_plot("Small", data_09_19_722513)
generate_gap_plot("All", data_09_19_722513)