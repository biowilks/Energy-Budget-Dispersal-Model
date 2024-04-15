rm(list=ls())
library(tidyverse)
library(igraph)
library(ggraph)
library(scales)

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

