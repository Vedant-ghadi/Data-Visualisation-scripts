# ============================================================================
# Figure 4: Sankey (Alluvial) Diagram
# Sheffield Clean Air Zone Analysis - Distributional Flow Visualization
# ============================================================================

# Load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(ggalluvial)

# Set working directory (adjust as needed)
setwd("c:/Users/vedan/.gemini/intro to data science")

# ============================================================================
# CONFIGURATION
# ============================================================================

CAZ_DATE <- as.Date("2023-02-27")
WHO_GUIDELINE <- 10  # μg/m³

# Pollution level colors (traffic light scheme)
colors_level <- c(
  "Safe (≤10)" = "#1A9850",      # Green - WHO compliant
  "Moderate (10-20)" = "#FEE08B", # Yellow - elevated
  "Unhealthy (>20)" = "#D73027"   # Red - unhealthy
)

# ============================================================================
# DATA LOADING AND PREPARATION
# ============================================================================

daily_data <- read_csv("sheffield_openmeteo/data/sheffield_daily.csv", 
                       show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(date), !is.na(no2_mean)) %>%
  mutate(
    # Period classification
    period = factor(if_else(date < CAZ_DATE, "Pre-CAZ", "Post-CAZ"), 
                    levels = c("Pre-CAZ", "Post-CAZ")),
    # Year as character for discrete axis
    year = as.character(year(date)),
    # Season classification
    season = factor(case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    ), levels = c("Winter", "Spring", "Summer", "Autumn")),
    # Pollution level classification
    pollution_level = factor(case_when(
      no2_mean <= WHO_GUIDELINE ~ "Safe (≤10)",
      no2_mean <= 20 ~ "Moderate (10-20)",
      TRUE ~ "Unhealthy (>20)"
    ), levels = c("Safe (≤10)", "Moderate (10-20)", "Unhealthy (>20)"))
  )

# ============================================================================
# SUMMARY STATISTICS BY POLLUTION LEVEL
# ============================================================================

level_summary <- daily_data %>%
  group_by(period, pollution_level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

message("Pollution Level Distribution by Period:")
print(level_summary)

# ============================================================================
# PREPARE ALLUVIAL DATA
# ============================================================================

alluvial_data <- daily_data %>%
  filter(year %in% c("2022", "2023", "2024")) %>%
  count(period, year, season, pollution_level, name = "freq")

message("\nAlluvial data prepared: ", nrow(alluvial_data), " flow categories")

# ============================================================================
# FIGURE 4: SANKEY DIAGRAM
# ============================================================================

fig4 <- ggplot(alluvial_data,
       aes(axis1 = period, 
           axis2 = year, 
           axis3 = season, 
           axis4 = pollution_level, 
           y = freq)) +
  # Alluvial flows
  geom_alluvium(aes(fill = pollution_level), 
                width = 1/5, 
                alpha = 0.8,
                curve_type = "quintic") +
  # Stratum rectangles
  geom_stratum(width = 1/5, 
               fill = "white", 
               color = "#333333", 
               linewidth = 0.7) +
  # Labels on strata
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)), 
            size = 3.8, 
            fontface = "bold", 
            color = "#333333") +
  # X-axis labels
  scale_x_discrete(
    limits = c("Period", "Year", "Season", "Pollution Level"), 
    expand = c(0.12, 0.05)
  ) +
  # Fill colors
  scale_fill_manual(values = colors_level, name = "Air Quality") +
  # Labels
  labs(
    title = "Alluvial Diagram: Period → Year → Season → Air Quality",
    subtitle = "Tracking daily pollution levels across periods, years, and seasons",
    caption = "Green = WHO compliant (≤10 μg/m³) | Yellow = Moderate (10-20) | Red = Unhealthy (>20)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, 
                              margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5, 
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5, 
                                margin = margin(t = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(25, 25, 25, 25),
    axis.text.x = element_text(size = 11, face = "bold", color = "#333333", 
                               margin = margin(t = 10))
  )

# Save figure
ggsave("sheffield_openmeteo/reports/fig4_sankey_diagram.png",
       fig4, width = 16, height = 10, dpi = 300, bg = "white")

message("\n✓ Saved: fig4_sankey_diagram.png")

# ============================================================================
# ADDITIONAL: SUMMARY STATISTICS FOR REPORT
# ============================================================================

message("\n", strrep("=", 50))
message("KEY FINDINGS FOR REPORT")
message(strrep("=", 50))

pre_safe <- level_summary %>% filter(period == "Pre-CAZ", pollution_level == "Safe (≤10)") %>% pull(pct)
post_safe <- level_summary %>% filter(period == "Post-CAZ", pollution_level == "Safe (≤10)") %>% pull(pct)
pre_unhealthy <- level_summary %>% filter(period == "Pre-CAZ", pollution_level == "Unhealthy (>20)") %>% pull(pct)
post_unhealthy <- level_summary %>% filter(period == "Post-CAZ", pollution_level == "Unhealthy (>20)") %>% pull(pct)

message("WHO Compliant Days (≤10 μg/m³):")
message("  Pre-CAZ: ", pre_safe, "%")
message("  Post-CAZ: ", post_safe, "%")
message("  Change: +", post_safe - pre_safe, " percentage points")

message("\nUnhealthy Days (>20 μg/m³):")
message("  Pre-CAZ: ", pre_unhealthy, "%")
message("  Post-CAZ: ", post_unhealthy, "%")
message("  Change: ", post_unhealthy - pre_unhealthy, " percentage points")
