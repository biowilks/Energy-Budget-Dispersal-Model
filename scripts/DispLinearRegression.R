library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db5 <- read.csv("DispersalTransformed.csv")

# Filtering for mean, median, and maximum distances
meandb <- filter(db5, Statistic == "Mean")
meddb <- filter(db5, Statistic == "Median")
maxdb <- filter(db5, Statistic == "Maximum")

#### Linear regression for mean ####
# Log10 transform
log10_Body.mass_mean <- log10(meandb$Body.mass)
log10_Distance_mean <- log10(meandb$Value)

# Linear regression model and extracting the slope (coefficient)
regression_mean  <- lm(log10_Distance_mean ~ log10_Body.mass_mean, data = meandb)
summary(regression_mean)

# Create a scatter plot 
scatter_plot_mean <- ggplot(meandb, aes(x = log10_Body.mass_mean, y = log10_Distance_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log10 Body mass (g)", y = "Log10 mean Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_mean)

#### Linear regression for median ####
# Log10 transform
log10_Body.mass_med <- log10(meddb$Body.mass)
log10_Distance_med <- log10(meddb$Value)

# Linear regression model and extracting the slope (coefficient)
regression_med  <- lm(log10_Distance_med ~ log10_Body.mass_med, data = meddb)
summary(regression_med)

# Create a scatter plot 
scatter_plot_med <- ggplot(meddb, aes(x = log10_Body.mass_med, y = log10_Distance_med)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log10 Body mass (g)", y = "Log10 median Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_med)

#### Linear regression for maximum ####
# Log10 transform
log10_Body.mass_max <- log10(maxdb$Body.mass)
log10_Distance_max <- log10(maxdb$Value)

# Linear regression model and extracting the slope (coefficient)
regression_max  <- lm(log10_Distance_max ~ log10_Body.mass_max, data = maxdb)
summary(regression_max)

# Create a scatter plot 
scatter_plot_max <- ggplot(maxdb, aes(x = log10_Body.mass_max, y = log10_Distance_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_max)



#### Linear regression for maximum by movement modes####
# Filtering for maximum distances and grouping by Movement Mode
maxmovdb <- filter(db5, Statistic == "Maximum") |>
  group_by(Movement.Mode)

maxmovdb <- maxmovdb |>
  filter(!is.na(Movement.Mode))

#### Linear regression for maximum (differentiated by Movement Mode) ####
# Log10 transform
maxmovdb <- maxmovdb |>
  mutate(log10_Body.mass = log10(Body.mass),
         log10_Distance = log10(Value))

# Linear regression model for each Movement Mode and extracting the slopes (coefficients)
regressions_maxmov <- maxmovdb |>
  group_by(Movement.Mode) |>
  do(model = lm(log10_Distance ~ log10_Body.mass, data = .))

# Summarize the regression models
summaries <- regressions_maxmov |>
  group_map(~ summary(.x$model))

# Create scatter plots for each Movement Mode
scatter_plots_maxmov <- maxmovdb |>
  ggplot(aes(x = log10_Body.mass, y = log10_Distance, color = Movement.Mode)) +
  geom_point(size = 1, alpha = 0.2) +  # Adjust size and transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum Dispersal distance (m)") +
  theme_minimal()

print(scatter_plots_maxmov)

# Print regression summaries
print(summaries)
