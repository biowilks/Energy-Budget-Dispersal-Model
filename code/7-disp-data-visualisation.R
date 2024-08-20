rm(list=ls())

# Load packages ----------
library("tidyverse")
library("viridis")
library("scales")

# Import dispersal function and empirical data----------
setwd(dirname(getActiveDocumentContext()$path))
source("6-disp-function.R")

setwd('C:/Users/xr49abiw/Documents/Energy-Budget-Model/output')
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



# Bioenergetic dispersal model predictions of maximum dispersal distance for each locomotion mode ----------

# Calculate dispersal distances for body masses present in empirical dataset
# Find unique body mass values from empirical data for each locomotion mode
body_mass_run <- unique(run_max$Body.mass)
body_mass_fly <- unique(fly_max$Body.mass)
body_mass_swim <- unique(swim_max$Body.mass)

# Calculate dispersal predictions for each locomotion mode and body mass, using disp_fun and unique body mass values
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


# Calculating percentage of empirical data above bioenergetic dispersal model predictions ----------
# Left join empirical data with model predictions based on body mass
run_max_merged <- left_join(run_max, ds.disprun, by = c("Body.mass" = "m_C")) %>%
  select(gbif.binomial, Value, disp_dist, Body.mass)
fly_max_merged <- left_join(fly_max, ds.dispfly, by = c("Body.mass" = "m_C")) %>%
  select(gbif.binomial, Value, disp_dist, Body.mass)
swim_max_merged <- left_join(swim_max, ds.dispswim, by = c("Body.mass" = "m_C")) %>%
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



# FIGURE 2a-d - The relationships between body mass and the parameters underlying the bioenergetic dispersal model ----------
## Plot parameters against body mass
#custom label needed for speed graph
custom_labels <- c(expression(10^2.5), expression(10^3), expression(10^3.5), expression(10^4), expression(10^4.5))
custom_breaks <- c(10^2.5, 10^3, 10^3.5, 10^4, 10^4.5)

vC_plot <- ggplot(ds.disprun, aes(x = m_C, y = v_C)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(breaks = custom_breaks, labels = custom_labels, trans = "log10") +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  geom_line(data = ds.disprun, aes(x = m_C, y = v_C), color = "chartreuse4", linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = v_C), color = "red", linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = v_C), color = "blue", linewidth = 1.5) +
  labs(y = "", x = "") +
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

BMR_plot <- ggplot(ds.disprun, aes(x = m_C, y = BMR, color = viridis(3)[3])) +
  geom_line(linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = BMR, color = viridis(3)[2]), linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = BMR, color = viridis(3)[1]),  linewidth = 1.5) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

COT_plot <- ggplot(ds.disprun, aes(x=m_C, y = COT)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  geom_line(data = ds.disprun,aes(x=m_C, y = COT),color = "chartreuse4",linewidth=1.5)+
  geom_line(data = ds.dispfly,aes(x=m_C, y = COT),color = "red",linewidth=1.5)+
  geom_line(data = ds.dispswim,aes(x=m_C, y = COT),color = "blue",linewidth=1.5)+
  labs(y= "", x = "")+
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

E0_plot <- ggplot(ds.disprun, aes(x = m_C, y = E_0, color = viridis(3)[3])) +
  geom_line( linewidth = 1.5) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = E_0, color = viridis(3)[2]), linewidth = 1.5) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = E_0, color = viridis(3)[1]), linewidth = 1.5) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "",
    x = "",
    color = "Species"
  ) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",size = 1.5, linetype = "solid"))+
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )



# FIGURE 4a-c - The relationship between maximum dispersal distance and body mass for a) flying birds, b) running mammals and c) swimming fish ----------
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
  labs(x = "", y = "") +
  scale_color_manual(name = "Locomotion Mode", values = mov_colour, guide = "none")+
  theme_minimal() +
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
  ggtitle("a)")+
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
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
  theme_minimal()  +
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
  ggtitle("b)")+
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.text = element_text(size = 35),
    axis.title = element_text(size = 35),
    title = element_text(size = 25, face = "bold"),
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
    breaks = c(1e0, 1e2, 1e4, 1e6),
    limits = c(1e0, 1e7)
  ) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = c(1e0, 1e2, 1e4, 1e6, 1e8),
    limits = c(1e0, 1e8)
  ) +
  ggtitle("c)")+
  theme(axis.line = element_line(colour = "grey20",linewidth = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        title = element_text(size = 25, face = "bold"),
        legend.position = "none"
  )



plot(scatter_plot_swim)


