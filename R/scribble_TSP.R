#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   scribble_TSP.R
# Description:   Create path images using TSP algorithms
# Author:        Brandon Monier
# Created:       2020-01-03 at 16:36:17
# Last Modified: 2020-01-03 at 16:58:06
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate transformed
#    images using a `geom_path()` approach via ggplot2 and the
#    TSP package.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(ggplot2)
# library(imager)
# library(reshape2)
library(scales)
library(stats)
library(TSP)
library(utils)


## Global parameters ----

### Image (change to adequate image if desired)
urlfile     <- "http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
file        <- "figs/frankenstein.jpg"
iter        <- 250
sample_size <- 400
outimg      <- "figs/frank_scribbles_TSP.png"


## Download image ----
if (!file.exists(file)) download.file(urlfile, destfile = file, mode = "wb")


## Load, convert to grayscale, filter image (to convert it to bw) and sample
X <- imager::load.image(file) %>%
    imager::grayscale() %>%
    as.matrix()

# Convert the matrix to data frame
dimnames(X) <- list(row = 1:nrow(X), col = 1:ncol(X))
X <- reshape2::melt(X)
colnames(X) <- c("x", "y", "value")

# Start ggplot
plot <- ggplot()

# This loop adds layers to the plot
pb <- progress_bar$new(
    format = "  -- Making paths [:bar] :percent eta: :eta",
    total = 100, clear = FALSE, width= 60)

for (i in seq_len(iter)) {
    pb$tick()
    # Weighted sample of pixels
    data <- X %>%
        dplyr::sample_n(sample_size, weight = 1 - value) %>%
        select(x, y)

    # Compute distances and solve TSP
    solution <- data %>%
        stats::dist() %>%
        TSP::as.TSP() %>%
        TSP::solve_TSP(method = "arbitrary_insertion")

    # Rearrange the original points according the TSP output
    data_to_plot <- data[solution, ]

    # Add a new layer to previous plot
    plot <- plot +
        geom_path(
            aes(x, y),
            data = data_to_plot,
            alpha = runif(1, min = 0, max = 0.1)
        )
}

# The final plot (at last)
plot +
    scale_y_continuous(trans=reverse_trans()) +
    coord_fixed() +
    theme_void()


## Do you like the result? Save it! (Change the filename if you want) ----
message("Saving TSP image...")
ggsave(
    filename = outimg,
    dpi      = 600,
    width    = 4,
    height   = 5,
    units    = "in"
)


