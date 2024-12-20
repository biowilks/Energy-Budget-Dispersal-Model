rm(list=ls())

# Load packages ----------
library("tidyverse")
library("viridis")
library("rstudioapi")
library("scales")

# Import data----------

setwd('~/Energy-Budget-Model/output')

conv_para <- read.csv("convertedparameters.csv")
conv_para_list <- setNames(conv_para$par_val_conv, conv_para$par_name)

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

# Define dispersal function with beta ----
## 1) INPUT VARIABLES
disp_fun_beta <- function(m_C,locomotion_mode,lambda,beta) {

  ## 2) PARAMETERS

  ## A) Energy storage
  if (locomotion_mode == "flying") {
    E_0 = conv_para_list[["a1"]] * m_C^conv_para_list[["b1"]] } else if (locomotion_mode == "running") {
      E_0 = conv_para_list[["a2"]] * m_C^conv_para_list[["b2"]] } else if (locomotion_mode == "swimming") {
        E_0 = conv_para_list[["a3"]] * m_C^conv_para_list[["b3"]] }

  ## Energy available for dispersal
  E_alpha = ((1-lambda) * E_0)


  ## B) Energy loss via dispersal

  ## Basal metabolic rate
  if (locomotion_mode == "flying") {
    BMR = conv_para_list[["a4"]] * m_C^conv_para_list[["b4"]] } else if (locomotion_mode == "running") {
      BMR = conv_para_list[["a5"]] * m_C^conv_para_list[["b5"]] } else if (locomotion_mode == "swimming") {
        BMR = conv_para_list[["a6"]] * m_C^conv_para_list[["b6"]] }

  ## Speed
  if(locomotion_mode == "flying") {
    v_0 = conv_para_list[["v_0_fly"]] } else if (locomotion_mode == "running"){
      v_0 = conv_para_list[["v_0_run"]] } else {v_0 = conv_para_list[["v_0_swim"]]}

  c = conv_para_list[["c"]]
  d = conv_para_list[["d"]]
  k = conv_para_list[["k"]]
  v_C = (((1/k)*m_C^c)/((m_C^(c+d)) + (1/(k*v_0))))

  ## Locomotion costs
  if (locomotion_mode == "flying") {
    LCOT = conv_para_list[["a7"]]*m_C^conv_para_list[["b7"]]*v_C^conv_para_list[["b8"]] + conv_para_list[["a8"]]*m_C^conv_para_list[["b7"]]*v_C^conv_para_list[["b9"]] + conv_para_list[["a9"]]*m_C^conv_para_list[["b10"]]*v_C^conv_para_list[["b9"]]}
  else if (locomotion_mode == "running") {LCOT = conv_para_list[["a10"]] * m_C^conv_para_list[["b11"]] * v_C}
  else if (locomotion_mode == "swimming") {LCOT = m_C*(conv_para_list[["a11"]]*exp(conv_para_list[["a12"]]*m_C^conv_para_list[["b12"]] * v_C))}

  ## Field metabolic rate while dispersing
  if(locomotion_mode == "flying") {
    FMR_disp = conv_para_list[["a13"]]* BMR + LCOT } else if (locomotion_mode == "running")
    {FMR_disp = conv_para_list[["a14"]]* BMR + LCOT} else {FMR_disp = BMR + LCOT}

  ## 3) OUTPUTS
  ## t_3 = time in s
  t_3 = E_alpha/ (FMR_disp*(1-beta) + BMR*beta)

  ## disp_dist = dispersal distance in m
  disp_dist_beta = (1-beta)*t_3*v_C


  ds.disp <- cbind(disp_dist_beta, t_3, beta)
  return(ds.disp)
}


# FIGURE S1. Sensitivity analyses showing the effect of changing the residual energy needed upon arrival (𝝀) on maximum dispersal distance predictions ----------
## lambda = % of energy storage needed for survival after dispersing

# Calculate dispersal predictions for each locomotion mode and body mass, using body mass range in empirical data
maximum_disp_dist1 <- function(locomotion_mode, lambda, beta) {
  if (locomotion_mode == "running") {
    body_mass_range <- body_mass_run
  } else if (locomotion_mode == "flying") {
    body_mass_range <- body_mass_fly
  } else if (locomotion_mode == "swimming") {
    body_mass_range <- body_mass_swim
  }

  disp_data <- data.frame()

  for (m_C in body_mass_range) {
    disp <- as.data.frame(disp_fun_beta(m_C,locomotion_mode = locomotion_mode, lambda = lambda, beta = 0))
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
  ds.disprun_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist1("running", lambda)
  ds.dispfly_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist1("flying", lambda)
  ds.dispswim_supp[[paste("lambda_", lambda, sep = "")]] <- maximum_disp_dist1("swimming", lambda)
}

# Flying
scatter_plot_fly_lambda <- ggplot(fly_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'red') +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'red') +

  geom_line(data = ds.dispfly_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = "0.1"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = "0.2"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = "0.3"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = "0.4"), linewidth = 1) +
  geom_line(data = ds.dispfly_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = "0.5"), linewidth = 1) +


  scale_color_manual(values = c("0.1" = "#660000", "0.2" = "#990000", "0.3" = "#CC0000", "0.4" = "#FF0000", "0.5" = "#FF6666")) +

  labs(x = "", y = "",color = "Residual energy (𝝀)")  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2,1e-1,1e0,1e1,1e2,1e3),
    limits = c(1e-2,1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6,1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  ggtitle("a) Flying birds") +
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_fly_lambda)

# Running
scatter_plot_run_lambda <- ggplot(run_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'palegreen4') +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'palegreen4') +


  geom_line(data = ds.disprun_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = '0.1'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = '0.2'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = '0.3'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = '0.4'), linewidth = 1) +
  geom_line(data = ds.disprun_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = '0.5'), linewidth = 1) +

  labs(x = "", y = "", color = "Residual energy (𝝀)") +
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
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_run_lambda)


# Swimming
scatter_plot_swim_lambda <- ggplot(swim_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = 'blue')+
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = 'blue') +


  geom_line(data = ds.dispswim_supp[["lambda_0.1"]], aes(x = m_C, y = disp_dist_0.1, color = '0.1'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.2"]], aes(x = m_C, y = disp_dist_0.2, color = '0.2'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.3"]], aes(x = m_C, y = disp_dist_0.3, color = '0.3'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.4"]], aes(x = m_C, y = disp_dist_0.4, color = '0.4'), linewidth = 1) +
  geom_line(data = ds.dispswim_supp[["lambda_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = '0.5'), linewidth = 1) +

  labs(x = "", y = "", color = "Residual energy (𝝀)") +
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
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_swim_lambda)


# Figure S2. Sensitivity analysis showing the effect of adding resting time (ꞵ) on maximum dispersal distance predictions ----------
# beta (ꞵ) is the time spent resting/hiding during dispersal (Energy loss = BMR)
# (1-beta) is the time spent active during dispersal (Energy loss = FMR)
# beta is between 0 and 1

maximum_disp_dist_beta <- function(locomotion_mode, lambda, beta) {
  if (locomotion_mode == "running") {
    body_mass_range <- body_mass_run
  } else if (locomotion_mode == "flying") {
    body_mass_range <- body_mass_fly
  } else if (locomotion_mode == "swimming") {
    body_mass_range <- body_mass_swim
  }

  disp_data <- data.frame()

  for (m_C in body_mass_range) {
    disp <- as.data.frame(disp_fun_beta(m_C, locomotion_mode = locomotion_mode, lambda = lambda, beta = beta))
    mass_disp <- cbind(m_C, disp)
    disp_data <- rbind(disp_data, mass_disp)
  }

  colnames(disp_data)[2] <- paste("disp_dist_", beta, sep = "")
  return(disp_data)
}

# Beta values for sensitivity analysis
betas <- c(0, 0.25, 0.5, 0.75)

# Collect data for each locomotion type with different beta values
ds.disprun_beta <- list()
ds.dispfly_beta <- list()
ds.dispswim_beta <- list()

for (beta in betas) {
  ds.disprun_beta[[paste("beta_", beta, sep = "")]] <- maximum_disp_dist_beta("running", lambda = 0.3, beta = beta)
  ds.dispfly_beta[[paste("beta_", beta, sep = "")]] <- maximum_disp_dist_beta("flying", lambda = 0.3, beta = beta)
  ds.dispswim_beta[[paste("beta_", beta, sep = "")]] <- maximum_disp_dist_beta("swimming", lambda = 0.3, beta = beta)
}


scatter_plot_fly_beta <- ggplot(fly_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = "red") +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = "red") +

  geom_line(data = ds.dispfly_beta[["beta_0"]], aes(x = m_C, y = disp_dist_0, color = "0"), linewidth = 1.2) +
  geom_line(data = ds.dispfly_beta[["beta_0.25"]], aes(x = m_C, y = disp_dist_0.25, color = "0.25"), linewidth = 1.2) +
  geom_line(data = ds.dispfly_beta[["beta_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = "0.5"), linewidth = 1.2) +
  geom_line(data = ds.dispfly_beta[["beta_0.75"]], aes(x = m_C, y = disp_dist_0.75, color = "0.75"), linewidth = 1.2) +

  scale_color_manual(values = c("0" = "#660000", "0.25" = "coral4",
                                "0.5" = "coral3", "0.75" = "lightcoral")) +

  labs(x = "", y = "", color = "Resting time (ꞵ)")  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
    limits = c(1e-2, 1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6, 1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  ggtitle("a) Flying birds") +
  theme(axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_fly_beta)

scatter_plot_run_beta <- ggplot(run_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = "palegreen4") +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = "palegreen4") +  # Main color for smooth line

  geom_line(data = ds.disprun_beta[["beta_0"]], aes(x = m_C, y = disp_dist_0, color = "0"), linewidth = 1.2) +
  geom_line(data = ds.disprun_beta[["beta_0.25"]], aes(x = m_C, y = disp_dist_0.25, color = "0.25"), linewidth = 1.2) +
  geom_line(data = ds.disprun_beta[["beta_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = "0.5"), linewidth = 1.2) +
  geom_line(data = ds.disprun_beta[["beta_0.75"]], aes(x = m_C, y = disp_dist_0.75, color = "0.75"), linewidth = 1.2) +

  scale_color_manual(values = c("0" = "#003300",
                                "0.25" = "#006600",
                                "0.5" = "palegreen3",
                                "0.75" = "palegreen1"))  +


labs(x = "", y = "", color = "Resting time (ꞵ)") +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
    limits = c(1e-2, 1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  ggtitle("b) Running birds") +
  theme(axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_run_beta)


scatter_plot_swim_beta <- ggplot(swim_max, aes(x = Body.mass, y = Value)) +
  geom_point(size = 2, alpha = 0.2, color = "blue") +
  geom_smooth(method = "gam", linetype = "dashed", se = TRUE, linewidth = 1.5, colour = "blue") +

  geom_line(data = ds.dispswim_beta[["beta_0"]], aes(x = m_C, y = disp_dist_0, color = "0"), linewidth = 1.2) +
  geom_line(data = ds.dispswim_beta[["beta_0.25"]], aes(x = m_C, y = disp_dist_0.25, color = "0.25"), linewidth = 1.2) +
  geom_line(data = ds.dispswim_beta[["beta_0.5"]], aes(x = m_C, y = disp_dist_0.5, color = "0.5"), linewidth = 1.2) +
  geom_line(data = ds.dispswim_beta[["beta_0.75"]], aes(x = m_C, y = disp_dist_0.75, color = "0.75"), linewidth = 1.2) +

  scale_color_manual(values = c("0" = "#000066", "0.25" = "cadetblue4",
                                "0.5" = "cadetblue3", "0.75" = "cadetblue1")) +

  labs(x = "", y = "", color = "Resting time (ꞵ)")  +
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3),
    limits = c(1e-2, 1e4)) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e2, 1e4, 1e6, 1e6, 1e8),
    limits = c(1e1, 1e8)) +
  theme_minimal() +
  ggtitle("c) Swimming") +
  theme(axis.line = element_line(colour = "grey20", linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 15, face = "bold"),
        legend.position = "right",
        legend.position.inside = c(.15, .8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )

print(scatter_plot_swim_beta)

