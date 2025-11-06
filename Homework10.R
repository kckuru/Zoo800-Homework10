##################
### Homework 10 ###
##################

# Group members: Keeley Kuru
# Date: 11/6/25

# ===== Objective 1 ===== #

####################################
## LTER Trout Bog Lake Color Data ##
####################################

# Hypothesis: Water color (absorbance) in Trout Bog Lake, WI has increased over time
# due to rising dissolved organic carbon (DOM) concentrations. 
# Expectation: Positive linear relationship between year and absorbance.

# Load libraries
library(tidyverse)
library(lubridate)

# Import data
data <- read_csv(here::here("data", "ntl87_v13.csv"))

# Filter for Trout Bog lake
tb_data <- data %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_data <- tb_data %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months: May (5) to August (8)
summer_tb <- tb_data %>%
  filter(month %in% 5:8)

# Normalize absorbance to 1 cm path length
summer_tb <- summer_tb %>%
  mutate(value_1cm = value / cuvette)

# Summarize by year
summer_summary <- summer_tb %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(value_1cm, na.rm = TRUE),
    sd_value = sd(value_1cm, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year)

print(summer_summary)

##############################
## B. Linear Regression Fit ##
##############################

# Fit linear model: mean absorbance (Y) vs. year (X)
lm_model <- lm(mean_value ~ year, data = summer_summary)
summary(lm_model)

# Extract model stats
model_summary <- summary(lm_model)
slope <- round(coef(lm_model)[2], 4)
p_value <- signif(model_summary$coefficients[2, 4], 2)
r_squared <- round(model_summary$r.squared, 2)

annotation_text <- paste0(
  "p = ", p_value, "\n",
  "R² = ", r_squared
)

#############################
## C. Evaluate Assumptions ##
#############################

# 1. Linearity: Residuals vs. Fitted plot
plot(lm_model, which = 1)
# → Residuals should be randomly scattered around 0; no clear curve or pattern.

# 2. Normality of residuals
plot(lm_model, which = 2)
# → Points should follow the 45° line. Deviations suggest non-normal residuals.

# 3. Homoscedasticity (equal variance)
plot(lm_model, which = 3)
# → Points should be evenly spread around zero. A funnel shape = unequal variance.

##########################
## D. Predictions ########
##########################

# Find median and 95th percentile of year
median_year <- median(summer_summary$year)
perc95_year <- quantile(summer_summary$year, 0.95)

# Predict mean absorbance and 95% prediction intervals
predictions <- predict(
  lm_model,
  newdata = data.frame(year = c(median_year, perc95_year)),
  interval = "prediction"
)

pred_df <- data.frame(
  year = c(median_year, perc95_year),
  predictions
)

print(pred_df)
# The predicted mean absorbance increases from 0.23 in 2008 to 0.33 in 2021, suggesting a positive temporal trend.
# The 95% prediction intervals are slightly wider at the upper percentile (2021), 
# meaning there is greater uncertainty in predictions farther from the dataset’s center.

##############
## Plotting ##
##############

abs_trends_over_years <- ggplot(summer_summary, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(color = "blue", size = 2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value)),
    label = annotation_text,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = unique(summer_summary$year)) +
  labs(
    title = "Summer (May–August) Color Trends for Trout Bog Lake",
    subtitle = "Mean absorbance normalized to a 1 cm pathlength",
    x = "Year",
    y = "Mean Absorbance (1 cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20", size = 10, angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

abs_trends_over_years


# ===== Objective 2 ===== #



