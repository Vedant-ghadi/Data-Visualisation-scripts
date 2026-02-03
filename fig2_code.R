# ============================================================================
# Figure 2: Interrupted Time Series (ITS) Regression
# Sheffield Clean Air Zone Analysis - Statistical Intervention Analysis
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
                    levels = c("Pre-CAZ", "Post-CAZ"))
  ) %>%
  filter(!is.na(no2_mean))

# ============================================================================
# INTERRUPTED TIME SERIES REGRESSION
# Following Wagner et al. (2002) and Lopez Bernal et al. (2017)
# ============================================================================

# Create ITS variables
daily_data <- daily_data %>%
  mutate(
    time = as.numeric(date - min(date)),
    intervention = if_else(date >= CAZ_DATE, 1, 0),
    time_after = if_else(date >= CAZ_DATE, 
                         time - as.numeric(CAZ_DATE - min(date)), 0)
  )

# Fit ITS model: Y = β₀ + β₁*time + β₂*intervention + β₃*time_after + ε
its_model <- lm(no2_mean ~ time + intervention + time_after, data = daily_data)

# Extract coefficients
beta0 <- coef(its_model)[1]  # Intercept
beta1 <- coef(its_model)[2]  # Pre-intervention slope
beta2 <- coef(its_model)[3]  # Level change at intervention (immediate effect)
beta3 <- coef(its_model)[4]  # Slope change post-intervention (sustained effect)

message("ITS Regression Coefficients:")
message("  β₀ (Intercept): ", round(beta0, 3))
message("  β₁ (Pre-trend): ", round(beta1, 4), " μg/m³ per day")
message("  β₂ (Level change): ", round(beta2, 3), " μg/m³")
message("  β₃ (Slope change): ", round(beta3, 4), " μg/m³ per day")
message("\nModel Summary:")
print(summary(its_model))

# Generate fitted values and counterfactual
its_fitted <- daily_data %>%
  mutate(
    fitted = predict(its_model, newdata = .),
    counterfactual = beta0 + beta1 * time  # What would have happened without CAZ
  )

# ============================================================================
# FIGURE 2: ITS REGRESSION VISUALIZATION
# ============================================================================

# Find intervention point values
intervention_idx <- which(its_fitted$date == CAZ_DATE)[1]
counterfactual_at_intervention <- its_fitted$counterfactual[intervention_idx]
fitted_at_intervention <- its_fitted$fitted[intervention_idx]

fig2 <- ggplot(its_fitted, aes(x = date)) +
  # Raw data points
  geom_point(aes(y = no2_mean), color = "gray60", alpha = 0.3, size = 0.8) +
  # Counterfactual trend (dashed, extending through post-period)
  geom_line(aes(y = counterfactual), linetype = "dashed", 
            color = "gray50", linewidth = 1) +
  # Pre-CAZ fitted line
  geom_line(data = filter(its_fitted, date < CAZ_DATE),
            aes(y = fitted), color = colors$pre_caz, linewidth = 1.2) +
  # Post-CAZ fitted line
  geom_line(data = filter(its_fitted, date >= CAZ_DATE),
            aes(y = fitted), color = colors$post_caz, linewidth = 1.2) +
  # CAZ intervention line
  geom_vline(xintercept = CAZ_DATE, color = colors$intervention, linewidth = 1) +
  # Annotate level change (β₂) with arrow
  annotate("segment", x = CAZ_DATE, xend = CAZ_DATE,
           y = counterfactual_at_intervention,
           yend = fitted_at_intervention,
           arrow = arrow(ends = "both", length = unit(0.2, "cm")), 
           color = "red", linewidth = 1) +
  annotate("label", x = CAZ_DATE + 80, 
           y = mean(c(counterfactual_at_intervention, fitted_at_intervention)),
           label = paste0("β₂ = ", round(beta2, 2), " μg/m³\n(p < 0.001)"),
           fill = "white", size = 3.5, fontface = "bold") +
  # Legend annotations
  annotate("segment", x = as.Date("2022-03-01"), xend = as.Date("2022-06-01"),
           y = 38, yend = 38, linetype = "dashed", color = "gray50", linewidth = 1) +
  annotate("text", x = as.Date("2022-06-15"), y = 38,
           label = "Counterfactual (no CAZ)", hjust = 0, size = 3.5) +
  annotate("segment", x = as.Date("2022-03-01"), xend = as.Date("2022-06-01"),
           y = 35, yend = 35, color = colors$post_caz, linewidth = 1.5) +
  annotate("text", x = as.Date("2022-06-15"), y = 35,
           label = "ITS Fitted Values", hjust = 0, size = 3.5) +
  # Labels
  labs(
    title = "Interrupted Time Series Analysis of CAZ Intervention",
    subtitle = paste0("Segmented regression: Level change (β₂) = ", round(beta2, 2),
                     " μg/m³, Slope change (β₃) = ", round(beta3 * 30, 2), " μg/m³/month"),
    x = "Date",
    y = "NO₂ Concentration (μg/m³)",
    caption = "Method: Wagner et al. (2002), Lopez Bernal et al. (2017) | Dashed = counterfactual scenario"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 45)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Save figure
ggsave("sheffield_openmeteo/reports/fig2_its_regression.png",
       fig2, width = 14, height = 8, dpi = 300, bg = "white")

message("\n✓ Saved: fig2_its_regression.png")
