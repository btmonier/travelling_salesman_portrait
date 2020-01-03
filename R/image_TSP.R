#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   image_TSP.R
# Description:   Create path images using TSP algorithms
# Author:        Brandon Monier
# Created:       2020-01-03 at 15:46:46
# Last Modified: 2020-01-03 at 16:08:54
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
library(scales)
library(stats)
library(TSP)
library(utils)


## Global parameters ----

### Image (change to adequate image if desired)
urlfile <- "http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"
file    <- "figs/frankenstein.jpg"
outimg  <- "figs/frankTSP.png"

### Sample size (for image points)
sample_size <- 20000


## Download image ----
if (!file.exists(file)) download.file(urlfile, destfile = file, mode = "wb")


## Load image ----

### Load, convert to grayscale, filter image (to convert it to bw) and sample
message("Loading image...")
data <- imager::load.image(file) %>%
  imager::grayscale() %>%
  imager::threshold("45%") %>%
  imager::as.cimg() %>%
  as.data.frame() %>%
  dplyr::sample_n(sample_size, weight = (1 - value)) %>%
  dplyr::select(x, y)



# === TSP Analysis ==================================================

## Compute distances and solve TSP (it may take a minute) ----
message("Solving TSP...")
solution <- data %>%
    stats::dist() %>%
    TSP::as.TSP() %>%
    TSP::solve_TSP(method = "arbitrary_insertion") %>%
    as.integer()


## Rearrange the original points according the TSP output ----
data_to_plot <- data[solution, ]



# === Visualize =====================================================

## A little bit of ggplot to plot results ----
data_to_plot %>%
    ggplot() +
        aes(x, y) +
        geom_path() +
        scale_y_continuous(trans = reverse_trans())+
        coord_fixed()+
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


