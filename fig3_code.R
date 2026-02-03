# ============================================================================
# Figure 3: Slope Graph with Statistical Significance
# Sheffield Clean Air Zone Analysis - Monthly Comparison
# ============================================================================

# Load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Set working directory (adjust as needed)
setwd("c:/Users/vedan/.gemini/intro to data science")

# ============================================================================
# CONFIGURATION
# ============================================================================

CAZ_DATE <- as.Date("2023-02-27")
WHO_GUIDELINE <- 10  # μg/m³

# Okabe-Ito colorblind-safe palette
colors <- list(
  pre_caz = "#0072B2",      # Blue
  post_caz = "#009E73",     # Green
  intervention = "#D55E00"  # Vermillion
)

# ============================================================================
# DATA LOADING AND PREPARATION
# ============================================================================

daily_data <- read_csv("sheffield_openmeteo/data/sheffield_daily.csv", 
                       show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    period = factor(if_else(date < CAZ_DATE, "Pre-CAZ", "Post-CAZ"),
                    levels = c("Pre-CAZ", "Post-CAZ")),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(!is.na(no2_mean))

# ============================================================================
# MONTHLY STATISTICS AND SIGNIFICANCE TESTING
# ============================================================================

# Calculate monthly means by period
monthly_stats <- daily_data %>%
  group_by(month, period) %>%
  summarise(mean_no2 = mean(no2_mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = mean_no2) %>%
  mutate(
    change = `Post-CAZ` - `Pre-CAZ`,
    pct_change = round(change / `Pre-CAZ` * 100, 1)
  )

# Perform t-tests for each month
monthly_tests <- daily_data %>%
  group_by(month) %>%
  summarise(
    n_pre = sum(period == "Pre-CAZ"),
    n_post = sum(period == "Post-CAZ"),
    p_value = tryCatch(
      t.test(no2_mean ~ period)$p.value,
      error = function(e) NA
    ),
    .groups = "drop"
  ) %>%
  mutate(
    significance = case_when(
      is.na(p_value) ~ "ns",
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# Combine statistics
monthly_stats <- left_join(monthly_stats, monthly_tests, by = "month")

message("Monthly NO₂ Changes:")
print(monthly_stats %>% select(month, `Pre-CAZ`, `Post-CAZ`, pct_change, significance))

# ============================================================================
# FIGURE 3: SLOPE GRAPH WITH SIGNIFICANCE
# ============================================================================

fig3 <- ggplot(monthly_stats) +
  # Connecting lines (slopes) colored by change magnitude
  geom_segment(aes(x = 1, xend = 2, 
                   y = `Pre-CAZ`, yend = `Post-CAZ`,
                   color = pct_change), 
               linewidth = 2, alpha = 0.8) +
  # Pre-CAZ points
  geom_point(aes(x = 1, y = `Pre-CAZ`), 
             size = 5, color = colors$pre_caz, 
             fill = "white", shape = 21, stroke = 2) +
  # Post-CAZ points
  geom_point(aes(x = 2, y = `Post-CAZ`), 
             size = 5, color = colors$post_caz, 
             fill = "white", shape = 21, stroke = 2) +
  # Month labels on left
  geom_text(aes(x = 0.85, y = `Pre-CAZ`, label = month), 
            hjust = 1, size = 4, fontface = "bold") +
  # Values and change % on right
  geom_text(aes(x = 2.15, y = `Post-CAZ`, 
                label = paste0(round(`Post-CAZ`, 1), " (", pct_change, "%)")),
            hjust = 0, size = 3.5) +
  # Significance stars
  geom_text(aes(x = 2.55, y = `Post-CAZ`, label = significance),
            hjust = 0, size = 5, fontface = "bold", color = "red") +
  # Color scale for change magnitude
  scale_color_gradient2(
    low = "#1A9850",      # Green for large decreases
    mid = "#FEE08B",      # Yellow for small changes
    high = "#D73027",     # Red for increases (shouldn't happen)
    midpoint = median(monthly_stats$pct_change),
    name = "% Change"
  ) +
  # X-axis
  scale_x_continuous(
    breaks = c(1, 2), 
    labels = c("Pre-CAZ\n(2022-Feb 2023)", "Post-CAZ\n(Mar 2023-2024)"),
    limits = c(0.4, 3)
  ) +
  # Y-axis
  scale_y_continuous(breaks = seq(8, 18, 2)) +
  # Labels
  labs(
    title = "Monthly NO₂ Changes: Pre-CAZ vs Post-CAZ",
    subtitle = "All 12 months show improvement | Significance: *** p<0.001, ** p<0.01, * p<0.05",
    x = NULL,
    y = "Mean NO₂ Concentration (μg/m³)",
    caption = "Slope direction indicates change; color intensity indicates magnitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold")
  )

# Save figure
ggsave("sheffield_openmeteo/reports/fig3_slope_significance.png",
       fig3, width = 12, height = 10, dpi = 300, bg = "white")

message("\n✓ Saved: fig3_slope_significance.png")
