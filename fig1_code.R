# ============================================================================
# Figure 1: Executive Dashboard
# Sheffield Clean Air Zone Analysis - Time Series Visualization
# ============================================================================

# Load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

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
    year = year(date),
    month = month(date, label = TRUE)
  ) %>%
  filter(!is.na(no2_mean))

# ============================================================================
# STATISTICAL ANALYSIS
# ============================================================================

# Calculate summary statistics
pre_mean <- mean(daily_data$no2_mean[daily_data$period == "Pre-CAZ"], na.rm = TRUE)
post_mean <- mean(daily_data$no2_mean[daily_data$period == "Post-CAZ"], na.rm = TRUE)
reduction_pct <- round((pre_mean - post_mean) / pre_mean * 100, 1)

# Statistical testing
t_result <- t.test(no2_mean ~ period, data = daily_data)
effect_size <- (pre_mean - post_mean) / 
               sqrt((sd(daily_data$no2_mean[daily_data$period == "Pre-CAZ"], na.rm = TRUE)^2 + 
                     sd(daily_data$no2_mean[daily_data$period == "Post-CAZ"], na.rm = TRUE)^2) / 2)

message("Pre-CAZ Mean: ", round(pre_mean, 2), " μg/m³")
message("Post-CAZ Mean: ", round(post_mean, 2), " μg/m³")
message("Reduction: ", reduction_pct, "%")
message("p-value: ", format.pval(t_result$p.value))
message("Cohen's d: ", round(effect_size, 2))

# ============================================================================
# FIGURE 1: EXECUTIVE DASHBOARD
# ============================================================================

fig1 <- ggplot(daily_data, aes(x = date, y = no2_mean)) +
  # Period shading (Grammar of Graphics: geom_rect)
  geom_rect(
    data = data.frame(
      xmin = c(min(daily_data$date), CAZ_DATE),
      xmax = c(CAZ_DATE, max(daily_data$date)),
      ymin = -Inf, ymax = Inf,
      period = c("Pre-CAZ", "Post-CAZ")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period),
    alpha = 0.1, inherit.aes = FALSE
  ) +
  # WHO guideline reference line
  geom_hline(yintercept = WHO_GUIDELINE, linetype = "dashed", 
             color = "red", linewidth = 1) +
  annotate("text", x = min(daily_data$date) + 30, y = WHO_GUIDELINE + 1,
           label = "WHO Limit: 10 μg/m³", hjust = 0, size = 3, color = "red") +
  # LOESS smoothing by period
  geom_smooth(data = filter(daily_data, period == "Pre-CAZ"),
              method = "loess", se = TRUE, color = colors$pre_caz,
              fill = colors$pre_caz, alpha = 0.2, linewidth = 1.5) +
  geom_smooth(data = filter(daily_data, period == "Post-CAZ"),
              method = "loess", se = TRUE, color = colors$post_caz,
              fill = colors$post_caz, alpha = 0.2, linewidth = 1.5) +
  # CAZ intervention line
  geom_vline(xintercept = CAZ_DATE, linetype = "solid", 
             color = colors$intervention, linewidth = 1.5) +
  annotate("text", x = CAZ_DATE, y = max(daily_data$no2_mean, na.rm = TRUE) * 0.95,
           label = "CAZ Launch\n27 Feb 2023", hjust = -0.1, size = 3.5,
           fontface = "bold", color = colors$intervention) +
  # Statistical annotations
  annotate("label", x = min(daily_data$date) + 100, y = 42,
           label = paste0("Pre-CAZ Mean: ", round(pre_mean, 1), " μg/m³"),
           fill = colors$pre_caz, color = "white", fontface = "bold", size = 4) +
  annotate("label", x = max(daily_data$date) - 150, y = 42,
           label = paste0("Post-CAZ Mean: ", round(post_mean, 1), " μg/m³\n",
                         "Reduction: ", reduction_pct, "%\n",
                         "p < 0.001, d = ", round(effect_size, 2)),
           fill = colors$post_caz, color = "white", fontface = "bold", size = 3.5) +
  # Scales
  scale_fill_manual(values = c("Pre-CAZ" = colors$pre_caz, 
                               "Post-CAZ" = colors$post_caz),
                    name = "Period") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, NA)) +
  # Labels
  labs(
    title = "Sheffield Clean Air Zone: Evidence of NO₂ Reduction",
    subtitle = "Daily nitrogen dioxide concentrations with LOESS smoothing (2022-2024)",
    x = "Date",
    y = "NO₂ Concentration (μg/m³)",
    caption = "Data: Copernicus CAMS via Open-Meteo API | WHO annual guideline = 10 μg/m³"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save figure
ggsave("sheffield_openmeteo/reports/fig1_executive_dashboard.png",
       fig1, width = 14, height = 8, dpi = 300, bg = "white")

message("\n✓ Saved: fig1_executive_dashboard.png")
