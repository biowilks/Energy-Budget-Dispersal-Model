library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db5 <- read.csv("DispersalTransformed.csv")

####Linear regression for all data ####
#log transform
log_Body.mass <- log(db5$Body.mass)
log_Distance <- log(db5$Value)

#Linear regression model and extracting the slope (coefficient)
regression  <- lm(log_Distance ~ log_Body.mass, data = db5)
summary(regression)

# Create a scatter plot 
scatter_plot <- ggplot(db5, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot)

####Linear regression for mammals####
# Filter data for mammals (n = 1567)
mammdb<- db5 |>
  filter(Taxon == "Mammal")

# Perform log transformation
mammdb$log_Body.mass <- log(mammdb$Body.mass)
mammdb$log_Distance <- log(mammdb$Value)

# Perform linear regression
regression_mamm <- lm(log_Distance ~ log_Body.mass, data = mammdb)
summary(regression_mamm)

# Create a scatter plot
scatter_plot_mamm <- ggplot(mammdb, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_mamm)


####Linear regression for birds####
# Filter data for bird (n = 2222)
bdb<- db5 |>
  filter(Taxon == "Bird")

# Perform log transformation
bdb$log_Body.mass <- log(bdb$Body.mass)
bdb$log_Distance <- log(bdb$Value)

# Perform linear regression
regression_b <- lm(log_Distance ~ log_Body.mass, data = bdb)
summary(regression_b)

# Create a scatter plot
scatter_plot_b <- ggplot(bdb, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_b)

####Linear regression for amphibian####
# Filter data for amphibian (n = 57)
adb<- db5 |>
  filter(Taxon == "Amphibian")

# Perform log transformation
adb$log_Body.mass <- log(adb$Body.mass)
adb$log_Distance <- log(adb$Value)

# Perform linear regression
regression_a <- lm(log_Distance ~ log_Body.mass, data = adb)
summary(regression_a)

# Create a scatter plot
scatter_plot_a <- ggplot(adb, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_a)

####Linear regression for fish####
# Filter data for Fish (n = 619)
fdb<- db5 |>
  filter(Taxon == "Fish")

# Perform log transformation
fdb$log_Body.mass <- log(fdb$Body.mass)
fdb$log_Distance <- log(fdb$Value)

# Perform linear regression
regression_f <- lm(log_Distance ~ log_Body.mass, data = fdb)
summary(regression_f)

# Create a scatter plot
scatter_plot_f <- ggplot(fdb, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_f)

####Linear regression for invertebrate####
# Filter data for invertebrate (n = 4)
idb<- db5 |>
  filter(Taxon == "Invertebrate")

# Perform log transformation
idb$log_Body.mass <- log(idb$Body.mass)
idb$log_Distance <- log(idb$Value)

# Perform linear regression
regression_i <- lm(log_Distance ~ log_Body.mass, data = idb)
summary(regression_i)

# Create a scatter plot
scatter_plot_i <- ggplot(idb, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_i)
