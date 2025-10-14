# For Better Or Worse ####
# RW Morris Oct 14th 2025
# 
# This script functions like a primitive run-file to import, preprocess and 
# analyse data. It will render a report (github pages) when complete.  
# 
# Setup ####
library(tidyverse)
library(lcmm)

# Import (not run) ####
# Import the data from HILDA, create a peri-stimulus time table for a life event
# (lenfw) and perform some preprocessing (e.g., join with demographics). 
# 
# source("R/import.R") # requires HILDA data (not provided)


# Fit models ####
# 
# Estimate growth mixture model (GMM) via hlme() in the lcmm package for lefnw
# (major financial worsening, e.g., bankruptcy). Save the results in /results
# 
# CAUTION: can take a long time to run (not recommended) 
source("R/lccm_ghmh_lefnw.R")
source("R/lccm_losat_lefnw.R")
# 
# e.g.,:
# 
# Fit a GMM for 3 classes, which allows non-linear growth using quadratic and 
# cubic trends, and can also handle unbalanced longitudinal data.  
#  
# hlme(ghmh ~ time + I(time^2) + I(time^3) + sex,
#       random =~ time + I(time^2) + I(time^3), # omit this line to remove RE 
#       subject = 'xwaveid', 
#       data = fdf, 
#       ng = 3,                                 # number of classes
#       nproc = 3,
#       mixture =~time + I(time^2) + I(time^3))
# 
# Including random =~ ... allows the model to include within-class variation 
# (e.g., Muthen & Shedden, 1999). Omit this line to assume no within-class 
# variation and approximate a latent class growth analysis (LCGA) as described 
# by Nagin (1999).

m3 <- read_rds("results/ghmh_lefnw_3group.rds")
postprob(m3)           

# Plot results ####
# 
# model-frame and required for plots below 
fdf <- read_rds("data/lefnw_mf.rds") |> 
  as.data.frame()

plot(m3, which="fit", var.time="time", marg=FALSE, shades = TRUE)  

