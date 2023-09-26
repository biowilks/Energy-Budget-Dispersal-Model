library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db5 <- read.csv("DispersalTransformed.csv")

#filtering for mean, median and maximum distances
meandb <- filter(db5, Statistic == "Mean")
meddb <- filter(db5, Statistic == "Median")
maxdb <- filter(db5, Statistic == "Maximum")

####Linear regression for mean####
#log transform
log_Body.mass_mean <- log(meandb$Body.mass)
log_Distance_mean <- log(meandb$Value)

#Linear regression model and extracting the slope (coefficient)
regression_mean  <- lm(log_Distance_mean ~ log_Body.mass_mean, data = meandb)
summary(regression_mean)

# Create a scatter plot 
scatter_plot_mean <- ggplot(meandb, aes(x = log_Body.mass_mean, y = log_Distance_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log mean Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_mean)

####Linear regression for median####
#log transform
log_Body.mass_med <- log(meddb$Body.mass)
log_Distance_med <- log(meddb$Value)

#Linear regression model and extracting the slope (coefficient)
regression_med  <- lm(log_Distance_med ~ log_Body.mass_med, data = meddb)
summary(regression_med)

# Create a scatter plot 
scatter_plot_med <- ggplot(meddb, aes(x = log_Body.mass_med, y = log_Distance_med)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log median Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_med)

####Linear regression for maximum####
#log transform
log_Body.mass_max <- log(maxdb$Body.mass)
log_Distance_max <- log(maxdb$Value)

#Linear regression model and extracting the slope (coefficient)
regression_max  <- lm(log_Distance_max ~ log_Body.mass_max, data = maxdb)
summary(regression_max)

# Create a scatter plot 
scatter_plot_max <- ggplot(maxdb, aes(x = log_Body.mass_max, y = log_Distance_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log maximum Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_max)


####Linear regression for mammals####
# Filter data for mammals maximum distance (n = 734)
mammdb<- maxdb |>
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

# Filter data for mammals mean distance (n = 734)
mammdb_mean<- meandb |>
  filter(Taxon == "Mammal")

# Perform log transformation
mammdb_mean$log_Body.mass <- log(mammdb_mean$Body.mass)
mammdb_mean$log_Distance <- log(mammdb_mean$Value)

# Perform linear regression
regression_mamm_mean <- lm(log_Distance ~ log_Body.mass, data = mammdb_mean)
summary(regression_mamm_mean)

# Create a scatter plot
scatter_plot_mamm_mean <- ggplot(mammdb_mean, aes(x = log_Body.mass, y = log_Distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Log Body mass (g)", y = "Log Dispersal distance (m)") +
  theme_minimal()

print(scatter_plot_mamm_mean)


####Linear regression for birds####
# Filter data for bird (n = 2665)
bdb<- maxdb |>
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
# Filter data for amphibian (n = 134)
adb<- maxdb |>
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
# Filter data for Fish (n = 171)
fdb<- maxdb |>
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
idb<- maxdb |>
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
