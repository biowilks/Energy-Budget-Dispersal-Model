rm(list=ls())

# Load packages ----------
library("rstudioapi")
library("tidyverse")
library("scales")
library("igraph")
library("ggraph")
library("grid")

library.dynam.unload()# Import energy function ----------
setwd("~/Energy-Budget-Model/code")
source("4-energy-function.R")

# FIGURE 3a - Absolute energy depletion across locomotion modes ----------
# Calculate energy  across different distances (for 2kg individual)
# create dataframes
ds.energyrun <- data.frame()
ds.energyfly <- data.frame()
ds.energyswim <- data.frame()

# filter to make sure energy remaining is greater than 0
energy <- function(m_C, locomotion_mode, disp_dist, lambda) {
  energy_df <- as.data.frame(energy_fun(m_C, locomotion_mode, disp_dist, lambda))
  if (any(energy_df$E_R < 0)) {
    return(NULL)
  } else {
    return(cbind(disp_dist, energy_df))
  }
}

# running
for (disp_dist in seq(0, 5000000, length = 2000)) {
  energy_result <- energy(m_C = 2, "running", disp_dist, 0.1)
  if (!is.null(energy_result)) {
    ds.energyrun <- rbind(ds.energyrun, energy_result)
  }
}

# flying
for (disp_dist in seq(0, 5000000, length = 2000)) {
  energy_result <- energy(m_C = 2, "flying", disp_dist, 0.1)
  if (!is.null(energy_result)) {
    ds.energyfly <- rbind(ds.energyfly, energy_result)
  }
}

# swimming
for (disp_dist in seq(0, 5000000, length = 2000)) {
  energy_result <- energy(m_C = 2, "swimming", disp_dist, 0.1)
  if (!is.null(energy_result)) {
    ds.energyswim <- rbind(ds.energyswim, energy_result)
  }
}


## plotting energy remaining against distance for different locomotion mode
energy<- ggplot(ds.energyrun, aes(x = disp_dist, y = E_R)) +
  geom_line(color = "chartreuse4", linewidth = 1.5) +
  scale_x_continuous()+
  scale_y_continuous()+
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",linewidth = 1, linetype = "solid"))+
  geom_line(data = ds.energyfly, aes(x = disp_dist, y = E_R), color = "red", linewidth = 1.5) +
  geom_line(data = ds.energyswim, aes(x = disp_dist, y = E_R), color = "blue", linewidth = 1.5) +
  labs(y = "", x = "")+
  ggtitle("") +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))

energy

# FIGURE 3b - Relative energy depletion for differently sized running animals ----------
# Calculate relative energy efficiency across different distances for small (4.5kg) and large (4000 kg)running mammals
# For different body masses
ds.energyrunsmall  <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 4.5,locomotion_mode = "running",disp_dist, lambda = 0))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunsmall = rbind(ds.energyrunsmall, mass_disp)
}

ds.energyrunlarge <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 4000,locomotion_mode = "running",disp_dist, lambda = 0))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunlarge = rbind(ds.energyrunlarge, mass_disp)
}

# Filter out negative values from both datasets
ds.energyrunsmall_filtered <- ds.energyrunsmall[ds.energyrunsmall$E_E >= 0, ]
ds.energyrunlarge_filtered <- ds.energyrunlarge[ds.energyrunlarge$E_E >= 0, ]

# Define breaks for the y-axis (using only the maximum value of the small dataset)
breaks <- max(ds.energyrunsmall_filtered$E_E)

# Define the range for the y-axis with extra space
y_range <- c(breaks, max(ds.energyrunlarge_filtered$E_E))

relative_energy <- ggplot() +
  geom_line(data = ds.energyrunsmall_filtered, aes(x = disp_dist, y = E_E), color = "#a4cc7dff", linewidth = 1.5) +
  geom_line(data = ds.energyrunlarge_filtered, aes(x = disp_dist, y = E_E), color = "#264805ff", linewidth = 1.5) +
  theme_minimal() +
  theme( axis.line = element_line(colour = "grey20",linewidth = 1, linetype = "solid"))+
  geom_hline(yintercept = 0.1,linetype = "dashed", linewidth = 1.5, ) +
  labs(y = "", x = "")+
  ggtitle("") +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))



relative_energy

# FIGURE 3c - Allometry of absolute dispersal costs across locomotion modes ----------
# Allometry of energy costs at 1m (J) i.e. absolute energy cost per meter
ds.energyrunabsolute  <- data.frame()

for(m_C in seq(0.01,1000, length = 100)) {
  disp = as.data.frame(energy_fun(disp_dist = 1, locomotion_mode = "running", m_C, lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.energyrunabsolute = rbind(ds.energyrunabsolute, mass_disp)
}

ds.energyflyabsolute  <- data.frame()

for(m_C in seq(0.01,1000, length = 100)) {
  disp = as.data.frame(energy_fun(disp_dist = 1, locomotion_mode = "flying", m_C, lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.energyflyabsolute = rbind(ds.energyflyabsolute, mass_disp)
}

ds.energyswimabsolute  <- data.frame()

for(m_C in seq(0.01,1000, length = 100)) {
  disp = as.data.frame(energy_fun(disp_dist = 1, locomotion_mode = "swimming", m_C, lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.energyswimabsolute = rbind(ds.energyswimabsolute, mass_disp)
}

# Filter out negative values from both datasets
ds.energyrunabsolute_filtered <- ds.energyrunabsolute[ds.energyrunabsolute $E_C >= 0, ]
ds.energyflyabsolute_filtered <- ds.energyflyabsolute[ds.energyflyabsolute $E_C >= 0, ]
ds.energyswimabsolute_filtered  <- ds.energyswimabsolute [ds.energyswimabsolute $E_C >= 0, ]

##plotting absolute energy remaining against mass for different locomotion_mode
absolute_energy <- ggplot(ds.energyrunabsolute_filtered, aes(x = m_C, y = E_M)) +
  geom_line(color = "chartreuse4", linewidth = 1.5) +
  theme_minimal() +
  geom_line(data = ds.energyflyabsolute_filtered, aes(x = m_C, y = E_M), color = "red", linewidth = 1.5) +
  geom_line(data = ds.energyswimabsolute_filtered, aes(x = m_C, y = E_M), color = "blue", linewidth = 1.5) +
  labs(y = "", x = "") +
  theme( axis.line = element_line(colour = "grey20",size = 1, linetype = "solid"))+
  scale_x_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggtitle("") +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    title = element_text(size = 20, face = "bold"))

absolute_energy


# FIGURE 4 - A dispersal-cost-weighted network for differently sized running animals ----------
### Calculating a dispersal-cost-weighted spatial network
#  Create matrix of distances and use the energy function to calculate relative energy remaining (E_E)
#  to represent the energy flow between patches, for each distance and for both small (4500) and large (40000000) mammals

# Set seed for reproducibility to create same network structure each time
set.seed(125)
# number of patches
n_p = 15 ### should now be dividable by the number of clusters n_c (below) for the clustered landscape
# coordinates random landscape
v_x = runif(n_p,min = 0, max = 1)
v_y = runif(n_p,min = 0, max = 1)

# cooridnates clustered landscape
n_c = 3 ## number of clusters
n_pc = (n_p-n_c)/n_c

# random cluster centers
#vc_x = runif(n_c,min = 0, max = 1)
#vc_y = runif(n_c,min = 0, max = 1)

# manual cluster centers
vc_x = c(0.2,0.5,0.8)
vc_y = c(0.8,0.2,0.5)

for (i in 1:n_c) {

  repeat {
    vc_x_temp = rnorm(n_pc, mean = vc_x[i], sd = 0.15)
    if (all(vc_x_temp >= 0 & vc_x_temp <= 1)) break
  }

  repeat {
    vc_y_temp = rnorm(n_pc, mean = vc_y[i], sd = 0.15)
    if (all(vc_y_temp >= 0 & vc_y_temp <= 1)) break
  }
  vc_x <- c(vc_x,vc_x_temp)
  vc_y <- c(vc_y,vc_y_temp)

}



# Calculate distance matrix

realised_max_distances <- 400000

coordinates <- data.frame(v_x, v_y)
dfdist <- as.matrix(dist(coordinates))
realised_matrix <- dfdist * realised_max_distances

coordinates_c <- data.frame(vc_x, vc_y)
dfdist_c <- as.matrix(dist(coordinates_c))
realised_matrix_c <- dfdist_c * realised_max_distances



# Function to calculate relative energy remaining for a given distance in m and mass (m_C) in g
calculate_energy_flow <- function(distance, m_C) {
  energy_output <- energy_fun(m_C = m_C, locomotion_mode = "running", disp_dist = distance, lambda = 0.1)
  E_E <- energy_output[, "E_E"]
  return(E_E)
}

# Calculate energy flow for small and large masses using the realised distance matrix created above and energy flow function
energy_flow_small <- apply(realised_matrix, MARGIN = c(1, 2), FUN = function(distance) calculate_energy_flow(distance, m_C = 4.5))
energy_flow_large <- apply(realised_matrix, MARGIN = c(1, 2), FUN = function(distance) calculate_energy_flow(distance, m_C = 4000))
energy_flow_small_c <- apply(realised_matrix_c, MARGIN = c(1, 2), FUN = function(distance) calculate_energy_flow(distance, m_C = 4.5))
energy_flow_large_c <- apply(realised_matrix_c, MARGIN = c(1, 2), FUN = function(distance) calculate_energy_flow(distance, m_C = 4000))
# Function to extract unique combinations of distance and energy flow
# i.e. removing duplicates of the same distances and adding in patch numbers (using upper.tri), both needed for graphical visualisation
extract_unique_combinations <- function(energy_flow) {
  upper_tri_indices <- upper.tri(energy_flow)
  from_patch <- rep(1:nrow(energy_flow), ncol(energy_flow))[upper_tri_indices]
  to_patch <- rep(1:ncol(energy_flow), each = nrow(energy_flow))[upper_tri_indices]
  distance <- as.vector(realised_matrix)[upper_tri_indices]
  energy_flow <- as.vector(energy_flow)[upper_tri_indices]

  energy_flow[energy_flow < 0] <- 0 # replaces negative values with 0, representing no energy flow between patches - to show impossible links

  data.frame(
    from_patch = from_patch,
    to_patch = to_patch,
    distance = distance,
    energy_flow = energy_flow
  )
}

#### calculating connectance and weighted connectance
energy_flow_small[energy_flow_small < 0]<-0
energy_flow_large[energy_flow_large < 0]<-0
energy_flow_small_c[energy_flow_small_c<0]<-0
energy_flow_large_c[energy_flow_large_c<0]<-0

binary_energy_flow_small  <-  energy_flow_small
binary_energy_flow_large  <-  energy_flow_large
binary_energy_flow_small_c<-energy_flow_small_c
binary_energy_flow_large_c<-energy_flow_large_c

binary_energy_flow_small  [binary_energy_flow_small > 0]<-1
binary_energy_flow_large  [binary_energy_flow_large > 0]<-1
binary_energy_flow_small_c[binary_energy_flow_small_c>0]<-1
binary_energy_flow_large_c[binary_energy_flow_large_c>0]<-1

connectance_small  <- sum(binary_energy_flow_small  )/n_p^2
connectance_large  <- sum(binary_energy_flow_large  )/n_p^2
connectance_small_c<- sum(binary_energy_flow_small_c)/n_p^2
connectance_large_c<- sum(binary_energy_flow_large_c)/n_p^2

weighted_connectance_small  <- sum(energy_flow_small  )/n_p^2
weighted_connectance_large  <- sum(energy_flow_large  )/n_p^2
weighted_connectance_small_c<- sum(energy_flow_small_c)/n_p^2
weighted_connectance_large_c<- sum(energy_flow_large_c)/n_p^2


# Create data frames for small and large energy flow
df_small <- extract_unique_combinations(energy_flow_small)
df_large <- extract_unique_combinations(energy_flow_large)
df_small_c <- extract_unique_combinations(energy_flow_small_c)
df_large_c <- extract_unique_combinations(energy_flow_large_c)

### Visualising the dispersal-cost-weighted spatial network
# using igraph

# Creating node attributes - using coordinates produced above
node_data <- data.frame(
  id = 1:n_p,
  color = "grey30",
  x = v_x,
  y = v_y
)

node_data_c <- data.frame(
  id = 1:n_p,
  color = "grey30",
  x = vc_x,
  y = vc_y
)

# Create graph objects
network_small <- graph_from_data_frame(df_small, directed = FALSE)
network_large <- graph_from_data_frame(df_large, directed = FALSE)
network_small_c <- graph_from_data_frame(df_small_c, directed = FALSE)
network_large_c <- graph_from_data_frame(df_large_c, directed = FALSE)

plot(network_small)
# Set up the layout
layout <- create_layout(network_small, layout = "auto")

# Create edge data frames with proper energy flow and coordinates for small edges
edge_data_small <- df_small %>%
  mutate(
    x = v_x[from_patch],
    y = v_y[from_patch],
    xend = v_x[to_patch],
    yend = v_y[to_patch],
    edge.id = 1:nrow(df_small),
    size = "Small"
  )

# Create edge data frames with proper energy flow and coordinates for large edges
edge_data_large <- df_large %>%
  mutate(
    x = v_x[from_patch],
    y = v_y[from_patch],
    xend = v_x[to_patch],
    yend = v_y[to_patch],
    edge.id = 1:nrow(df_large),
    size = "Large"
  )

# Create edge data frames with proper energy flow and coordinates for small edges
edge_data_small_c <- df_small_c %>%
  mutate(
    x = vc_x[from_patch],
    y = vc_y[from_patch],
    xend = vc_x[to_patch],
    yend = vc_y[to_patch],
    edge.id = 1:nrow(df_small_c),
    size = "Small Clustered"
  )

# Create edge data frames with proper energy flow and coordinates for large edges
edge_data_large_c <- df_large_c %>%
  mutate(
    x = vc_x[from_patch],
    y = vc_y[from_patch],
    xend = vc_x[to_patch],
    yend = vc_y[to_patch],
    edge.id = 1:nrow(df_large_c),
    size = "Large Clustered"
  )

# Combine edge data frames
edge_data_combined <- bind_rows(edge_data_small, edge_data_large,edge_data_small_c, edge_data_large_c)

# Calculate edge widths based on the energy flow for small dataset
edge_data_combined$width <- ifelse(edge_data_combined$size == "Small" & edge_data_combined$energy_flow > 0,
                                   (edge_data_combined$energy_flow - min(df_small$energy_flow)) /
                                     (max(df_large$energy_flow) - min(df_small$energy_flow)) * 100,
                                   0)

# Calculate edge widths based on the energy flow for large dataset
edge_data_combined$width <- ifelse(edge_data_combined$size == "Large" & edge_data_combined$energy_flow > 0,
                                   (edge_data_combined$energy_flow - min(df_small$energy_flow)) /
                                     (max(df_large$energy_flow) - min(df_small$energy_flow)) * 100,
                                   edge_data_combined$width)

# Calculate edge widths based on the energy flow for small dataset
edge_data_combined$width <- ifelse(edge_data_combined$size == "Small Clustered" & edge_data_combined$energy_flow > 0,
                                   (edge_data_combined$energy_flow - min(df_small_c$energy_flow)) /
                                     (max(df_large_c$energy_flow) - min(df_small_c$energy_flow)) * 100,
                                   0)

# Calculate edge widths based on the energy flow for large dataset
edge_data_combined$width <- ifelse(edge_data_combined$size == "Large Clustered" & edge_data_combined$energy_flow > 0,
                                   (edge_data_combined$energy_flow - min(df_small_c$energy_flow)) /
                                     (max(df_large_c$energy_flow) - min(df_small_c$energy_flow)) * 100,
                                   edge_data_combined$width)

# Plot for small animal energy flow
connectance_small
connectance_large
connectance_small_c
connectance_large_c
weighted_connectance_small
weighted_connectance_large
weighted_connectance_small_c
weighted_connectance_large_c


p1 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined |> filter(size == "Small" & energy_flow > 0),
    aes(width = energy_flow),
    color = "#a4cc7dff",
    lineend = "round",
    linejoin = "round",
    alpha = 1
  ) +
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


# Plot for large animal energy flow
p2 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined |> filter(size == "Large" & energy_flow > 0),
    aes(width = energy_flow),
    color = "#264805ff",
    lineend = "round",
    linejoin = "round",
    alpha = 1
  ) +
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

# Plot for large animal energy flow
p3 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined |> filter(size == "Small Clustered" & energy_flow > 0),
    aes(width = energy_flow),
    color = "#a4cc7dff",
    lineend = "round",
    linejoin = "round",
    alpha = 1
  ) +
  geom_node_point(
    data = node_data_c,
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

p3

# Plot for large animal energy flow
p4 <- ggraph(layout, layout = "auto") +
  geom_edge_link(
    data = edge_data_combined |> filter(size == "Large Clustered" & energy_flow > 0),
    aes(width = energy_flow),
    color = "#264805ff",
    lineend = "round",
    linejoin = "round",
    alpha = 1
  ) +
  geom_node_point(
    data = node_data_c,
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
  labs(title = "") +
  # Add the scale bar
  geom_segment(aes(x = 0.1, xend = 0.3, y = 0.05, yend = 0.05), linewidth = 1.5, color = "black")

p4

