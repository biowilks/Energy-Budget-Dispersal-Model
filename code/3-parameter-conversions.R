### Task: simplify Table 1 by converting parameters into common units
### e.g. normalisation constants for flying, running, swimming all expressed in same unit.

rm(list=ls())


# Load packages -----------------------------------------------------------

library("tidyverse")

# Desired units for parameters

# Body mass, M_C_kg: kg
# Energy storage, E_0: J
# Basal metabolic rate, BMR: J h^-1
# Field metabolic rate, FMR_disp: J h^-1
# Gross cost of transport, COT: J m^-1
# Movement speed, v_C: m h^-1
# Time, t: h
# Max dispersal distance, disp_dist:  m

# Define existing conversion factors ------------------------------------------------------

# kg -> kJ
gamma1 <- 40*10^6

# kJ -> J 
gamma2 <- 1000

# ml 02 h^-1 -> J h^-1
gamma3 <- 20

# mg 02 h^-1 -> J h^-1
gamma4 <- 1.33

# m s^-1 -> m h^-1
gamma_5 <- 3600

# kg -> g
gamma_mass <- 1000


# Convert each constant to desired unit -----------------------------------

# Convert constants function: 
# Input: (normalisation constant, optional product of conversion factors, exponent, optional mass conversion)
# Output: List with two entries: output[[1]] = converted norm constant; output[[2]]] = allometric exponent
convert_constant <- function(a, gammas = 1, b, mass_conversion = 1) {
  a_converted <- a * gammas * mass_conversion # Create a single lumped parameter by multiplying the constants 
  return(list(a_converted = a_converted, b = b))
}

# Energy storage, E_0 ----------------------------------------------------------
# Converted constant expressed in J kg^-b
# Could also express in kilocalorie (1 Joules = 4184 kcal approx.)

# Birds energy storage
a1 <- 10^(-1.2) # original ref is g g^-b therefore dimensionless constant
b1 <- 0.98
E0_birds <- convert_constant(a1, gamma1, b1) 

# Mammals energy storage
a2 <- 10^(-1.3) # original ref is g g^-b therefore dimensionless constant
b2 <- 1
E0_mammals <- convert_constant(a2, gamma1, b2) # gamma2 kJ -> J conversion is not needed

# Fish energy storage
a3 <- 10^(0.62)
b3 <- 0.02 + 1 # Multiply energy density by mass to get total energy storage (i.e. M^b * M^1 = M^[b+1])
E0_fish <- convert_constant(a3, gamma2, b3, mass_conversion = gamma_mass)  # Convert mass to kg Check THIS!!!

# Basal Metabolic Rate (BMR) -------------------------------------------------------------------------

# Birds BMR
a4 <- 7.434
b4 <- 0.648
BMR_birds <- convert_constant(a4, gamma3, b4)

# Mammals BMR
a5 <- 3.248
b5 <- 0.735
BMR_mammals <- convert_constant(a5, gamma3, b5)

# Fish BMR
a6 <- 10^1.87
b6 <- 0.95
BMR_fish <- convert_constant(a6, b6, gamma3/gamma4) # Multiply by both gamma parameters; Check if body mass conversion is correct!!!

# Step 4: Convert Cost of Transport (COT)
# Flying
a7 <- 3.6
b7 <- 0.69
COT_flying <- convert_constant(a7, 1, b7, 1)

# Running
a8 <- 10.7
b8 <- 0.68
COT_running <- convert_constant(a8, 1, b8, 1)

# Swimming
a9 <- 1.1
b9 <- 0.62
COT_swimming <- convert_constant(a9, 1, b9, 1)

# Table of original versus converted parameter values ------------------------
# Converted constant has same units as output i.e. if output is in J, then converted constant is in J kg^-b (I'm not sure about the mass units)

# Table of parameter values
pars_df <- data.frame(c(rep("E0: Energy storage [J]", 3), rep("BMR: Basal metabolic rate [J/h]", 3), rep("COT: Cost of transport[J/m]", 3)),
                   c("7a","7b","7c", "8a","8b","8c", "9a","9b","9c"), 
                   c("Birds", "Mammals", "Fish", "Birds", "Mammals", "Fish", "Flying", "Running", "Swimming"),
                   c(b1,b2,b3,b4,b5,b6,b7,b8,b9),
                   c(a1,a2,a3,a4,a5,a6,a7,a8,a9),
                   c(E0_birds[[1]], E0_mammals[[1]], E0_fish[[1]], BMR_birds[[1]], BMR_mammals[[1]], BMR_fish[[1]], COT_flying[[1]], COT_running[[1]], COT_swimming[[1]])
)
colnames(pars_df) <- c("Output", "Equation", "Group", "Exponent", "Original.Constant", "Converted.Constant.J")

# Add Constant express in kcal, kcal/h, kcal/m (1 Kilocalorie = 4184 Joules)
pars_df <- pars_df %>% mutate(Converted.Const.kJ = signif(Converted.Constant.J*0.001, 4),
                              Converted.Const.kcal = signif(Converted.Constant.J*0.0023907, 4))

# Tibble is just easier to read
tibble(pars_df)
