rm(list=ls())

# Load packages ----------
library("tidyverse")
library("viridis")
library("rstudioapi")
library("scales")

# Import dispersal function and empirical data----------
setwd("~/Energy-Budget-Model/code")
source("6-disp-function.R")

setwd('~/Energy-Budget-Model/output')
dispdata <- read.csv("DispersalTransformed.csv")

#   Removing NA, NaN and Inf values
dispdatamax <- dispdata |>
  filter(!is.na(Value) & !is.infinite(Value) & !is.nan(Value),
         !is.na(Body.mass) & !is.infinite(Body.mass) & !is.nan(Body.mass),
         !is.na(Locomotion.mode) )

# database summary
dispdatamax |>
  pull(MetaRef) |>
  n_distinct()

#  Filtering for each locomotion mode
run_max<- dispdatamax|> filter (Locomotion.mode == "Running")
fly_max <- dispdatamax |> filter (Locomotion.mode == "Flying")
swim_max <- dispdatamax |> filter (Locomotion.mode == "Swimming")

# FIGURE 2a-d - The relationships between body mass and the parameters underlying the bioenergetic dispersal model ----------
# Bioenergetic model prediction across a body mass range
bioenergeticpredictions <- function(locomotion_mode) {
  if (locomotion_mode == "running") {
    body_mass_range <- seq(0.01, 5000, length.out = 100)
  } else if (locomotion_mode == "flying") {
    body_mass_range <- seq(0.01, 50, length.out = 100)
  } else if (locomotion_mode == "swimming") {
    body_mass_range <- seq(0.001, 500, length.out = 100)
  }
  maximum_disp_dist <- data.frame()
  for (m_C in body_mass_range) {
    disp <- as.data.frame(disp_fun(m_C, locomotion_mode = locomotion_mode, lambda = 0.1))
    mass_disp <- cbind(m_C, disp)
    maximum_disp_dist <- rbind(maximum_disp_dist, mass_disp)
  }

  return(maximum_disp_dist)
}


# maximum dispersal distance predictions
ds.disprun <- bioenergeticpredictions("running")
ds.dispfly <- bioenergeticpredictions("flying")
ds.dispswim <- bioenergeticpredictions("swimming")


## Plot parameters against body mass
vC_plot <- ggplot(ds.disprun, aes(x = m_C, y = v_C)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
                limits = c(1e-3, 1e3)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3)) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid")) +
  geom_line(data = ds.disprun, aes(x = m_C, y = v_C), color = "chartreuse4", linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = v_C), color = "red", linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = v_C), color = "blue", linewidth = 1.5) +
  labs(y = "", x = "") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

BMR_plot <- ggplot(ds.disprun, aes(x = m_C, y = BMR, color = viridis(3)[3])) +
  geom_line(linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = BMR, color = viridis(3)[2]), linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = BMR, color = viridis(3)[1]),  linewidth = 1.5) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
                limits = c(1e-3, 1e3)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3)) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

LCOT_plot <- ggplot(ds.disprun, aes(x=m_C, y = LCOT)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
                limits = c(1e-3, 1e3)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3)) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  geom_line(data = ds.disprun,aes(x=m_C, y = LCOT),color = "chartreuse4",linewidth=1.5)+
  geom_line(data = ds.dispfly,aes(x=m_C, y = LCOT),color = "red",linewidth=1.5)+
  geom_line(data = ds.dispswim,aes(x=m_C, y = LCOT),color = "blue",linewidth=1.5)+
  labs(y= "", x = "")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

E0_plot <- ggplot(ds.disprun, aes(x = m_C, y = E_0, color = viridis(3)[3])) +
  geom_line( linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = E_0, color = viridis(3)[2]), linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = E_0, color = viridis(3)[1]), linewidth = 1.5) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
                limits = c(1e-3, 1e3)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = c(1e2, 1e4, 1e6, 1e8,1e10)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "",
    x = "",
    color = "Species"
  ) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

# FIGURE 5a-c - The relationship between maximum dispersal distance and body mass for a) flying birds, b) running mammals and c) swimming fish ----------
# Find unique body mass values from empirical data for each locomotion mode
body_mass_run <- unique(run_max$Body.mass)
body_mass_fly <- unique(fly_max$Body.mass)
body_mass_swim <- unique(swim_max$Body.mass)

# Calculate dispersal predictions for each locomotion mode and body mass, using body mass range in empirical data
maximum_disp_dist <- function(body_mass, locomotion_mode) {
  maximum_disp_dist <- data.frame()
  for (m_C in body_mass) {
    disp <- as.data.frame(disp_fun(m_C, locomotion_mode = locomotion_mode, lambda = 0.1))
    mass_disp <- cbind(m_C, disp)
    maximum_disp_dist <- rbind(maximum_disp_dist, mass_disp)
  }
  return(maximum_disp_dist)
}

# maximum dispersal distance predictions
modelmamm <- maximum_disp_dist(body_mass_run, "running")
modelbird <- maximum_disp_dist(body_mass_fly, "flying")
modelfish <- maximum_disp_dist(body_mass_swim, "swimming")

###Creating plots for each locomotion mode
#setting colours
mov_colour <- c('Flying' = 'red', 'Swimming' = 'blue', 'Running' = 'palegreen4',
                'Model Flying' ='#660000', 'Model Swimming' = '#000066', 'Model Running' = '#003300')

##flying birds
scatter_plot_fly <- fly_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Locomotion.mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",  linetype = "dashed", se = T, linewidth = 1.5) +
  geom_line(data = modelbird, aes(x = m_C, y = disp_dist), linewidth = 1.5, color = mov_colour['Model Flying']) +
  labs(x = "", y = "")  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  scale_color_manual(name = "Locomotion Mode", values = mov_colour, guide = "none")+
  theme_minimal() +
 ggtitle("a) Flying birds") +
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
    legend.position = "none"
  )

plot(scatter_plot_fly)


##running mammals
scatter_plot_run <- run_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Locomotion.mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5) +
  geom_line(data = modelmamm, aes(x = m_C, y = disp_dist), linewidth = 1.5, color = mov_colour['Model Running']) +
  labs(x = "", y = "") +
  scale_color_manual(name = "Locomotion Mode", values = mov_colour, guide = "none") +
  theme_minimal() +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  ggtitle("b) Running mammals") +
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "none"
  )

plot(scatter_plot_run)

##swimming fish
scatter_plot_swim <- swim_max |>
  ggplot(aes(x = Body.mass, y = Value, color = Locomotion.mode)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5,) +
  geom_line(data = modelfish, aes(x = m_C, y = disp_dist), linewidth = 1.5, color = mov_colour['Model Swimming']) +
  labs(x = "", y = "") +
  scale_color_manual(name = "Locomotion Mode", values = mov_colour, guide = "none")+
  theme_minimal()  +
  scale_x_log10(
   labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  ggtitle("c) Swimming fish") +
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "none"
  )

plot(scatter_plot_swim)


# Calculating percentage of empirical data above bioenergetic dispersal model predictions ----------
# Left join empirical data with model predictions based on body mass
run_max_merged <- left_join(run_max, modelmamm, by = c("Body.mass" = "m_C")) %>%
  select(gbif.binomial, Value, disp_dist, Body.mass)
fly_max_merged <- left_join(fly_max, modelbird, by = c("Body.mass" = "m_C")) %>%
  select(gbif.binomial, Value, disp_dist, Body.mass)
swim_max_merged <- left_join(swim_max, modelfish, by = c("Body.mass" = "m_C")) %>%
  select(gbif.binomial, Value, disp_dist, Body.mass)

# calculate the percentage of data points exceeding model predictions
percentage_exceeding <- function(empirical_data, model_predictions) {
  exceeding_counts <- sum(empirical_data > model_predictions, na.rm = TRUE)
  total_data_points <- sum(!is.na(empirical_data))
  percentage_exceeding <- exceeding_counts / total_data_points * 100
  return(percentage_exceeding)
}

# for each locomotion mode
run_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))

fly_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))

swim_max_merged %>%
  summarise(percentage_exceeding = percentage_exceeding(Value, disp_dist))

