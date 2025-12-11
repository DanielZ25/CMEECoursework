# GPDD_Data.R
# Map GPDD population time series locations using the maps package

rm(list = ls())

# Install/load maps package
if (!require(maps)) {
    install.package("maps")
    library(maps)
} else {
    library(maps)
}

# Load GPDD filtered dataset

load("../data/GPDDFiltered.RData")


# Remove rows with missing coordinates
gpdd_sub <- gpdd[!is.na(gpdd$lat) & !is.na(gpdd$long), ]

# Draw a world map
map("world",
    fill = TRUE,
    col = "grey90",
    bg  = "white",
    mar = c(0, 0, 0, 0))

# Add GPDD points
points(gpdd_sub$long,
       gpdd_sub$lat,
       pch = 20,
       col = "red",
       cex = 0.5)

# Bias: GPDD locations are heavily focused on Europe and North America,
# with very few data points from the tropics, Africa, South America and Asia.
# This creates a strong spatial bias, meaning analyses based on GPDD are
# dominated by temperate, well-studied regions and are not globally
# representative.


