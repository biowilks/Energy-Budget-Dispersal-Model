rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(cowplot)
library(viridis)

###IMPORTING DATA AND FUNCTIONS NEEDED FOR DATA VISUALISATION####
#Importing energy budget model function
setwd(dirname(getActiveDocumentContext()$path))
source("5-disp-function.R")

#Importing empirical data
setwd('C:/Users/xr49abiw/Documents/Energy-Budget-Model/output')
dispdata <- read.csv("DispersalTransformed.csv")

##  Filtering data for maximum dispersal distance data for running mammals, flying birds and swimming fish
#   Removing NA, NaN and Inf values
dispdatamax <- dispdata |>
  filter(Movement.Mode != "Mixed" & Family_gbif != "Delphinidae" & Family_gbif != "Physeteridae"&
           ((Class_gbif == "Mammalia" & Movement.Mode == "Running") |
              (Class_gbif == "Aves" & Movement.Mode == "Flying") | Movement.Mode == "Swimming")) |>
  filter(Statistic == "Maximum") |>
  filter(!is.na(Value) & !is.infinite(Value) & !is.nan(Value),
         !is.na(Body.mass) & !is.infinite(Body.mass) & !is.nan(Body.mass),
         !is.na(Movement.Mode) )

#  filtering for each movement mode
run_max<- dispdatamax|> filter (Movement.Mode == "Running") |> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
fly_max <- dispdatamax |> filter (Movement.Mode == "Flying")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
swim_max <- dispdatamax |> filter (Movement.Mode == "Swimming")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)

#  summarising dataset
dispdatamax |>
  pull(Reference) |>
  n_distinct()

###MODEL PREDICTIONS FOR EACH MOVEMENT MODE####
# Calculate dispersal distances for body massess present in empirical dataset
# Find unique body mass values from empirical data for each movement mode
body_mass_run <- unique(run_max$Body.mass)
body_mass_fly <- unique(fly_max$Body.mass)
body_mass_swim <- unique(swim_max$Body.mass)

# Calculate dispersal predictions for each movement mode and body mass, using disp_fun and unique body mass values
maximum_disp_dist <- function(body_mass, movement_mode) {
  maximum_disp_dist <- data.frame()
  for (m_C in body_mass) {
    disp <- as.data.frame(disp_fun(m_C, movement_mode = movement_mode, lambda = 0.1))
    mass_disp <- cbind(m_C, disp)
    maximum_disp_dist <- rbind(maximum_disp_dist, mass_disp)
  }
  return(maximum_disp_dist)
}

# maximum dispersal distance predictions
ds.disprun <- maximum_disp_dist(body_mass_run, "running")
ds.dispfly <- maximum_disp_dist(body_mass_fly, "flying")
ds.dispswim <- maximum_disp_dist(body_mass_swim, "swimming")

#Rename for plotting
modelmamm <- ds.disprun |>
  select(m_C,disp_dist)

modelbird <- ds.dispfly |>
  select(m_C,disp_dist)

modelfish<- ds.dispswim |>
  select(m_C,disp_dist)

# Model summary for each movement mode
model.disprun <- lm(log10(disp_dist) ~ log10(m_C), data = ds.disprun)
summary(model.disprun)

model.dispfly <- lm(log10(disp_dist) ~ log10(m_C), data = ds.dispfly)
summary(model.dispfly)

model.dispswim <- lm(log10(disp_dist) ~ log10(m_C), data = ds.dispswim)
summary(model.dispswim)



###CALCULATING PERCENTAGE OF DATA ABOVE MODEL PREDICTIONS####
# Left join empirical data with model predictions based on body mass
run_max_merged <- left_join(run_max, ds.disprun, by = c("Body.mass" = "m_C")) %>%
  select(Species_ID_gbif, Value, disp_dist, Body.mass)
fly_max_merged <- left_join(fly_max, ds.dispfly, by = c("Body.mass" = "m_C")) %>%
  select(Species_ID_gbif, Value, disp_dist, Body.mass)
swim_max_merged <- left_join(swim_max, ds.dispswim, by = c("Body.mass" = "m_C")) %>%
  select(Species_ID_gbif, Value, disp_dist, Body.mass)

# calculate the percentage of data points exceeding model predictions
percentage_exceeding <- function(empirical_data, model_predictions) {
  exceeding_counts <- sum(empirical_data > model_predictions, na.rm = TRUE)
  total_data_points <- sum(!is.na(empirical_data))
  percentage_exceeding <- exceeding_counts / total_data_points * 100
  return(percentage_exceeding)
}

# for each movement mode
run_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))

fly_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))

swim_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))



###FIGURE 2a-d####
## Plot parameters against body mass
#custom label needed for speed graph
custom_labels <- c(expression(10^2.5), expression(10^3), expression(10^3.5), expression(10^4), expression(10^4.5))
custom_breaks <- c(10^2.5, 10^3, 10^3.5, 10^4, 10^4.5)

vC_plot <- ggplot(ds.disprun, aes(x = m_C, y = v_C)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(breaks = custom_breaks, labels = custom_labels, trans = "log10") +
  theme_minimal() +
  geom_line(data = ds.disprun, aes(x = m_C, y = v_C), color = "chartreuse4", linewidth = 1) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = v_C), color = "red", linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = v_C), color = "blue", linewidth = 1) +
  labs(y = "Movement speed (m/h)", x = "Body size (g)") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))


BMR_plot <- ggplot(ds.disprun, aes(x = m_C, y = BMR, color = viridis(3)[3]),  linewidth = 1) +
  geom_line() +
  geom_line(data = ds.dispfly, aes(x = m_C, y = BMR, color = viridis(3)[2]), linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = BMR, color = viridis(3)[1]),  linewidth = 1) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "Basal metabolic rate (J/h)",
    x = "Body size (g)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

BMR_plot

COT_plot <- ggplot(ds.disprun, aes(x=m_C, y = COT)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_minimal() +
  geom_line(data = ds.disprun,aes(x=m_C, y = COT),color = "chartreuse4",linewidth=1)+
  geom_line(data = ds.dispfly,aes(x=m_C, y = COT),color = "red",linewidth=1)+
  geom_line(data = ds.dispswim,aes(x=m_C, y = COT),color = "blue",linewidth=1)+
  labs(y= "Cost of transport (J/m)", x = "Body size (g)")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))

E0_plot <- ggplot(ds.disprun, aes(x = m_C, y = E_0, color = viridis(3)[3]), linewidth = 1) +
  geom_line() +
  geom_line(data = ds.dispfly, aes(x = m_C, y = E_0, color = viridis(3)[2]), linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = E_0, color = viridis(3)[1]), linewidth = 1) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "Basal metabolic rate (J/h)",
    x = "Body size (g)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

E0_plot

plot_grid(E0_plot, BMR_plot, labels = c('a','b'))
plot_grid(COT_plot,vC_plot, labels = c('c','d'))

###FIGURE 4a-c####
###Creating plots for each movement mode
#setting colours
mov_colour <- c('Flying' = 'red', 'Swimming' = 'blue', 'Running' = 'palegreen4',
                'Model Flying' ='#660000', 'Model Swimming' = '#000066', 'Model Running' = '#003300')

##flying birds
scatter_plot_fly <- fly_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Movement.Mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",  linetype = "dashed", se = T) +
  geom_line(data = modelbird, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Flying']) +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum dispersal distance (m)") +
  scale_color_manual(name = "Movement Mode", values = mov_colour, guide = "none")+
  theme_minimal_grid() +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6),
    limits = c(1e0, 1e7)
  ) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6, 1e8),
    limits = c(1e0, 1e8)
  ) +
  ggtitle("a")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

plot(scatter_plot_fly)


##running mammals
scatter_plot_run <- run_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Movement.Mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",linetype = "dashed", se = T) +
  geom_line(data = modelmamm, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Running']) +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum dispersal distance (m)") +
  scale_color_manual(name = "Movement Mode", values = mov_colour, guide = "none") +
  theme_minimal_grid()  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6),
    limits = c(1e0, 1e7)
  ) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6, 1e8),
    limits = c(1e0, 1e8)
  ) +
  ggtitle("b")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )


plot(scatter_plot_run)

##swimming fish
scatter_plot_swim <- swim_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Movement.Mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",linetype = "dashed", se = T) +
  geom_line(data = modelfish, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Swimming']) +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum dispersal distance (m)") +
  scale_color_manual(name = "Movement Mode", values = mov_colour, guide = "none")+
  theme_minimal_grid()  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6),
    limits = c(1e0, 1e7)
  ) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6, 1e8),
    limits = c(1e0, 1e8)
  ) +
  ggtitle("c")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

plot(scatter_plot_swim)

plot_grid(scatter_plot_fly,scatter_plot_run,scatter_plot_swim, nrow = 1)

