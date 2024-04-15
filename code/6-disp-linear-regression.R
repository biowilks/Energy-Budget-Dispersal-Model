rm(list=ls())

library(tidyverse)
library(cowplot)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

### Importing empirical data
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

##filtering for each movement mode
run_max<- dispdatamax|> filter (Movement.Mode == "Running") |> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
fly_max <- dispdatamax |> filter (Movement.Mode == "Flying")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
swim_max <- dispdatamax |> filter (Movement.Mode == "Swimming")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)

###Importing theoretical predictions
modelbird <-read.csv("flydispdistance.csv")
modelmamm<-read.csv("rundispdistance.csv")
modelfish<-read.csv("swimdispdistance.csv")


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


##combined graph
scatter_plots_allmovmod <- dispdatamax |>
  ggplot(aes(x = Body.mass, y = Value, color = Movement.Mode)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_line(data = modelmamm, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Running']) +
  geom_line(data = modelbird, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Flying']) +
  geom_line(data = modelfish, aes(x = m_C, y = disp_dist), linewidth = 1, color = mov_colour['Model Swimming']) +
  labs(x = "Log10 Body mass (g)", y = "Log10 maximum Dispersal distance (m)") +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(name = "Movement Mode", values = mov_colour) +
  theme_minimal()

print(scatter_plots_allmovmod)
