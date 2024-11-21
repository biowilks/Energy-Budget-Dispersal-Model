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

#  Filtering for each locomotion mode
run_max<- dispdatamax|> filter (Locomotion.mode == "Running")
fly_max <- dispdatamax |> filter (Locomotion.mode == "Flying")
swim_max <- dispdatamax |> filter (Locomotion.mode == "Swimming")

# Find unique body mass values from empirical data for each locomotion mode
body_mass_run <- unique(run_max$Body.mass)
body_mass_fly <- unique(fly_max$Body.mass)
body_mass_swim <- unique(swim_max$Body.mass)


################## Sensitivity Analysis Resting ########################
# alpha is the time spent resting/hiding during dispersal (Energy loss = BMR)
# (1-alpha) is the time spent active during dispersal (Energy loss = FMR)
# alpha is between 0 and 1

# Calculate dispersal predictions for each locomotion mode and body mass, using body mass range in empirical data
maximum_disp_dist2 <- function(body_mass, locomotion_mode) {
  maximum_disp_dist <- data.frame()
  for (m_C in body_mass) {
    for(alpha in seq(0,0.75, by = 0.25)) {
      disp <- as.data.frame(disp_fun(m_C, locomotion_mode = locomotion_mode, lambda = 0.1, alpha = alpha))
      mass_disp <- cbind(m_C, disp)
      maximum_disp_dist <- rbind(maximum_disp_dist, mass_disp)
    }
  }
  return(maximum_disp_dist)
}

# maximum dispersal distance predictions
modelmamm2 <- maximum_disp_dist2(body_mass_run, "running")
modelbird2 <- maximum_disp_dist2(body_mass_fly, "flying")
modelfish2 <- maximum_disp_dist2(body_mass_swim, "swimming")

###Creating plots for each locomotion mode
#setting colours
mov_colour <- c('Flying' = 'coral3', 'Swimming' = 'cadetblue3', 'Running' = 'palegreen3',
                'Model Flying' ='coral4', 'Model Swimming' = 'cadetblue4', 'Model Running' = 'palegreen4')


##flying birds
cols.alpha.fly <- c("coral4", "coral3", "lightcoral",  "sandybrown")

scatter_plot_fly <- fly_max |>
  ggplot(aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, col = mov_colour['Flying']) +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5, col = mov_colour['Flying']) +
  geom_line(data = modelbird2, aes(x = m_C, y = disp_dist, colour = factor(alpha)), linewidth = 1.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "resting time", values = cols.alpha.fly) +
  theme_minimal() +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  ggtitle("a) Flying birds") +
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(.75, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

plot(scatter_plot_fly)


##running mammals
cols.alpha.run <- c("palegreen4", "palegreen3", "palegreen1",  "olivedrab1")

scatter_plot_run <- run_max |>
  ggplot(aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, col = mov_colour['Running']) +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5, col = mov_colour['Running']) +
  geom_line(data = modelmamm2, aes(x = m_C, y = disp_dist, colour = factor(alpha)), linewidth = 1.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "resting time", values = cols.alpha.run) +
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
        legend.position = "inside",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )


plot(scatter_plot_run)

##swimming fish
cols.alpha.swim <- c("cadetblue4", "cadetblue3", "cadetblue1",  "lightblue1")

scatter_plot_swim <- swim_max |>
  ggplot(aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, col = mov_colour['Swimming']) +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5, col = mov_colour['Swimming']) +
  geom_line(data = modelfish2, aes(x = m_C, y = disp_dist, colour = factor(alpha)), linewidth = 1.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "resting time", values = cols.alpha.swim)+
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
        legend.position = "inside",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

plot(scatter_plot_swim)

library(cowplot)
plot_grid(scatter_plot_fly,
          scatter_plot_run,
          scatter_plot_swim,
          ncol =3)

ggsave("plot_max.dist_alpha.png", height = 5, width = 15, bg = "white")


######################## Sensitivity analyses for lambda ########################
maximum_disp_dist_supp <- function(locomotion_mode, lambda) {
  if (locomotion_mode == "running") {
    body_mass_range <- body_mass_run
  } else if (locomotion_mode == "flying") {
    body_mass_range <- body_mass_fly
  } else if (locomotion_mode == "swimming") {
    body_mass_range <- body_mass_swim
  }

  disp_data <- data.frame()

  for (m_C in body_mass_range) {
    disp <- as.data.frame(disp_fun(m_C,locomotion_mode = locomotion_mode, lambda = lambda, alpha = 0))
    mass_disp <- cbind(m_C, disp)
    disp_data <- rbind(disp_data, mass_disp)
  }

  colnames(disp_data)[2] <- paste("disp_dist_", lambda, sep = "")
  return(disp_data)
}

# Lambdas
lambdas <- c(0.1, 0.2, 0.3, 0.4, 0.5)

ds.disprun_supp <- list()
ds.dispfly_supp <- list()
ds.dispswim_supp <- list()

for (lambda in lambdas) {
  ds.disprun_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist_supp("running", lambda)
  ds.dispfly_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist_supp("flying", lambda)
  ds.dispswim_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist_supp("swimming", lambda)
}

# Flying
scatter_plot_fly_supp <- ggplot(fly_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'red') +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'red') +

  geom_line(data = ds.dispfly_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = "0.1"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = "0.2"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = "0.3"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = "0.4"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = "0.5"), linewidth = 1) +


  scale_color_manual(name = "Lambda", values = c("0.1" = "#660000", "0.2" = "#990000", "0.3" = "#CC0000", "0.4" = "#FF0000", "0.5" = "#FF6666")) +

  labs(x = "", y = "")  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  ggtitle("a) Flying birds")+
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
  )

print(scatter_plot_fly_supp)

# Running
scatter_plot_run_supp <- ggplot(run_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'palegreen4') +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'palegreen4') +


  geom_line(data = ds.disprun_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = '0.1'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = '0.2'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = '0.3'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = '0.4'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = '0.5'), linewidth = 1) +

  labs(x = "", y = "", color = "Lambda") +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
    limits = c(1e-2, 1e4)
  ) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e8),
    limits = c(1e1, 1e8)
  ) +
  scale_color_manual(values = c("0.1" = '#003300', "0.2" = '#006600', "0.3" = '#009900', "0.4" = '#33CC66', "0.5" = '#99FFCC')) +
  theme_minimal() +
  ggtitle("b) Running") +
  theme(
    axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid"),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 15),
    )

print(scatter_plot_run_supp)


# Swimming
scatter_plot_swim_supp <- ggplot(swim_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'blue')+
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'blue') +


  geom_line(data = ds.dispswim_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = '0.1'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = '0.2'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = '0.3'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = '0.4'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = '0.5'), linewidth = 1) +

  labs(x = "", y = "", color = "Lambda") +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  scale_color_manual(values = c("0.1" = '#000066', "0.2" = '#0000CC', "0.3" = '#0000FF', "0.4" = '#3366FF', "0.5" = '#66CCFF')) +
  theme_minimal() +
  ggtitle("c) Swimming") +
  theme(
    axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid"),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 15),
  )

print(scatter_plot_swim_supp)


# Shark points on the graph -------
scatter_plot_shark <- swim_max |>
  ggplot(aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.5, color = 'blue') +
  geom_smooth(method = "gam",linetype = "dashed", se = T,linewidth = 1.5,color = 'blue') +
  geom_line(data = ds.dispswim_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1), linewidth = 1.5, color = '#000066') +
  labs(x = "", y = "") +
  theme_minimal()  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  ggtitle("Shark points")+
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        title = element_text(size = 25, face = "bold"),
        legend.position = "none"
  )


scatter_plot_shark <- scatter_plot_shark +
  geom_point(data = swim_max[swim_max$class == "Elasmobranchii", ],
             aes(x = Body.mass, y = Value),
             color = "red", size = 2, alpha = 0.5)

plot(scatter_plot_shark)
