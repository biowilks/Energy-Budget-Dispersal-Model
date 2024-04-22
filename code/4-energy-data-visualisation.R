rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(cowplot)
library(ggbreak)
library(scales)
library(igraph)
library(ggraph)

setwd(dirname(getActiveDocumentContext()$path))
source("3-energy-function.R")

###FIGURE 3a####
# Calculate energy  across different distances (for ~ 2kg  individual)
# For each movement mode
ds.energyrun  <- data.frame()

for(disp_dist in seq(0,5000000
                     , length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "running", disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrun = rbind(ds.energyrun, mass_disp)
}

ds.energyfly  <- data.frame()

for(disp_dist in seq(0,5000000
                     , length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "flying",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyfly = rbind(ds.energyfly, mass_disp)
}

ds.energyswim  <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "swimming",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyswim = rbind(ds.energyswim, mass_disp)
}

##plotting energy remaining against distance for different movement modes
energy <- ggplot(ds.energyrun, aes(x = disp_dist, y = E_R)) +
  geom_line(color = "chartreuse4", linewidth = 1) +
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma, limits=c(0, 10000000))+
  theme_minimal() +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(data = ds.energyfly, aes(x = disp_dist, y = E_R), color = "red", linewidth = 1) +
  geom_line(data = ds.energyswim, aes(x = disp_dist, y = E_R), color = "blue", linewidth = 1) +
  labs(y = "Energy remaining (J)", x = "Distance (m)")+
  ggtitle("") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))

energy

###FIGURE 3b####
# Calculate energy  across different distances for running mammals
# For different body masses
ds.energyrunsmall  <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 45000,movement_mode = "running",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunsmall = rbind(ds.energyrunsmall, mass_disp)
}

ds.energyrunlarge <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 4000000,movement_mode = "running",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunlarge = rbind(ds.energyrunlarge, mass_disp)
}

# Filter out negative values from both datasets
ds.energyrunsmall_filtered <- ds.energyrunsmall[ds.energyrunsmall$E_R >= 0, ]
ds.energyrunlarge_filtered <- ds.energyrunlarge[ds.energyrunlarge$E_R >= 0, ]

# Define breaks for the y-axis (using only the maximum value of the small dataset)
breaks <- max(ds.energyrunsmall_filtered$E_R)

# Define the range for the y-axis with extra space
y_range <- c(breaks, max(ds.energyrunlarge_filtered$E_R))

p <- ggplot() +
  geom_line(data = ds.energyrunsmall_filtered, aes(x = disp_dist, y = E_R), color = "#a4cc7dff", size = 1) +
  geom_line(data = ds.energyrunlarge_filtered, aes(x = disp_dist, y = E_R), color = "#4c900aff", size = 1) +
  geom_hline(yintercept = breaks, color = "white", size = 1) +
  geom_hline(yintercept = 90000000, linetype = "dashed", color = "black", size = 0.1) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 2400000, by = 200000)) +
  scale_y_continuous(labels = scales::comma) +
  scale_y_cut(breaks = breaks, space = 0) +
  coord_cartesian(ylim = y_range) +
  theme_minimal() +
  labs(y = "Energy remaining (J)", x = "Distance (m)")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))


p

###FIGURE 3c####



###FIGURE 3d####
# Set seed for reproducibility
set.seed(124)
#number of patches
n_p = 5
#coordinates
v_x = runif(n_p,min = 0, max = 1)
v_y = runif(n_p,min = 0, max = 1)

# Calculate distance matrix
coordinates <- data.frame(v_x, v_y)
dfdist <- as.matrix(dist(coordinates))
realised_max_distances <- 150000.0
realised_matrix <- dfdist * realised_max_distances

#energetic costs (J) for running mammal small (4500g) and large (4000000g)
energetic_cost_small <- data.frame(
  from_patch = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
  to_patch = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
  distance = c(65594.86,71353.94,105665.52,21040.54,21222.71,50871.38,53498.43,67202.50,54117.15,99954.86),
  energetic_cost = c(3342905,3636404,5385022,1072287,1081571,2592553,2726435,3424835,2757967,5093990)
)

energetic_cost_large <- data.frame(
  from_patch = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
  to_patch = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
  distance = c(65594.86,71353.94,105665.52,21040.54,21222.71,50871.38,53498.43,67202.50,54117.15,99954.86),
  energetic_cost = c(249961393,271907437,402658083,80178884,80873077,193854839,203865701,256087604,206223448,380896553)
)

# Creating node attributes
node_data <- data.frame(
  id = 1:n_p,
  color = "grey30",
  x = v_x,
  y = v_y
)

# Create graph objects
network_small <- graph_from_data_frame(energetic_cost_small, directed = FALSE)
network_large <- graph_from_data_frame(energetic_cost_large, directed = FALSE)

# Set up the layout
layout <- create_layout(network_small, layout = "auto")

# Create edge data frames with proper energetic costs
edge_data_small <- energetic_cost_small %>%
  mutate(
    x = v_x[from_patch],
    y = v_y[from_patch],
    xend = v_x[to_patch],
    yend = v_y[to_patch],
    edge.id = 1:nrow(energetic_cost_small)
  )

edge_data_large <- energetic_cost_large %>%
  mutate(
    x = v_x[from_patch],
    y = v_y[from_patch],
    xend = v_x[to_patch],
    yend = v_y[to_patch],
    edge.id = 1:nrow(energetic_cost_large)
  )

# Combine edge data frames
edge_data_combined <- bind_rows(
  mutate(edge_data_small, size = "Small"),
  mutate(edge_data_large, size = "Large")
)

#edge
edge_width_limits <- range(edge_data_combined$energetic_cost)

#colours
colors <- c('Small' = 'gold', 'Large' = 'darkgoldenrod4')

colors <- c('1072287' = '#F8E473',
            '1081571' = '#F8DE7E',
            '2592553' = '#FADA5E',
            '2726435' = '#FFD300',
            '2757967' = '#FFBF00',
            '3342905' = '#DBA520',
            '3424835' = '#EB9605',
            '3636404' = '#EF820D',
            '5093990' = '#FF7417',
            '5385022' = '#FC6600',
            '80178884' = '#B1560F',
            '80873077' = '#8B4000',
            '193854839' = '#7C4700',
            '203865701' = '#7F461B',
            '206223448' = '#793802',
            '249961393' = '#622A0F',
            '256087604' = '#5C2C06',
            '271907437'= '#48260D',
            '380896553'= '#362312',
            '402658083'= '#2B1700'
)

# Define edge widths
edge_widths <- c('1072287' = 1.4,
                 '1081571' = 1.8,
                 '2592553' = 2.2,
                 '2726435' = 2.6,
                 '2757967' = 3.0,
                 '3342905' = 3.4,
                 '3424835' = 3.8,
                 '3636404' = 4.2,
                 '5093990' = 4.6,
                 '5385022' = 5.0,
                 '80178884' = 5.4,
                 '80873077' = 5.8,
                 '193854839' = 6.2,
                 '203865701' = 6.6,
                 '206223448' = 7,
                 '249961393' = 7.4,
                 '256087604' = 7.8,
                 '271907437' = 8.2,
                 '380896553' = 8.6,
                 '402658083' = 9
)

edge_data_combined$width <- edge_widths[as.character(edge_data_combined$energetic_cost)]
edge_data_combined$color <- colors[as.character(edge_data_combined$energetic_cost)]
edge_data_combined$color <- colors[edge_data_combined$size]

p1 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined %>% filter(size == "Small"),
    aes(alpha = energetic_cost,
        color = color,
        width = energetic_cost),
    lineend = "round",
    linejoin = "round"
  ) +
  scale_edge_color_identity() +
  scale_edge_width(limits = edge_width_limits) +
  geom_node_point(
    data = node_data,
    aes(x = x, y = y, fill = color),
    shape = 21,
    size = 15,
    color = "grey35"
  ) +
  scale_fill_identity() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) +
  theme(legend.position = "none") +
  labs(title = "")

p1

p2 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined %>% filter(size == "Large"),
    aes(alpha = energetic_cost,
        color = color,
        width = energetic_cost),
    lineend = "round",
    linejoin = "round"
  ) +
  scale_edge_color_identity() +
  scale_edge_width(limits = edge_width_limits) +
  geom_node_point(
    data = node_data,
    aes(x = x, y = y, fill = color),
    shape = 21,
    size = 15,
    color = "grey35"
  ) +
  scale_fill_identity() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) +
  theme(legend.position = "none") +
  labs(title = "")

p2

