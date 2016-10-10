#=================================================================================================================================================
# STEP 0. Package Setup and Initialization 
#=================================================================================================================================================
# Load packages
library(dplyr)
library(DBI)
library(tidyr)
# For text mining
library(NLP)
library(tm)
library(SnowballC)
library(ggplot2); theme_set(theme_bw())

# Set local path 
setwd("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Advanced Data Analysis /ADA_Simpson_text_analysis/")

# Load data 
# Characters' names data 
datraw.characters <- read.csv("original_data/simpsons_characters.csv", header = TRUE)
# Episodes data 
datraw.episodes <- read.csv("original_data/simpsons_episodes.csv", 
                            stringsAsFactors = FALSE, header = TRUE) # .csv files need to be saved as MS-DOS Comma Separated files 
# Location data 
datraw.locations <- read.csv("original_data/simpsons_locations.csv", header = TRUE)
# Script lines data (text)
datraw.scripts <- read.csv("original_data/simpsons_script_lines.csv", # .csv files need to be saved as MS-DOS Comma Separated files 
                           stringsAsFactors = FALSE, header = TRUE)

#=================================================================================================================================================
# STEP 1. Data Processing 
#=================================================================================================================================================
# Make copies of original datasets 
dat.characters <- datraw.characters %>% select(-c(gender, normalized_name))
dat.episodes <- datraw.episodes %>% rename(episode_id = id)
dat.locations <- datraw.locations %>% select(-normalized_name)
dat.scripts <- datraw.scripts %>% select(-c(normalized_text, X:X.13, raw_text))

# Save as RDS (avoid loading .csv missing values)
saveRDS(dat.characters, "exported_data/dat_characters.rds")
saveRDS(dat.episodes, "exported_data/dat_episodes.rds")
saveRDS(dat.locations, "exported_data/dat_locations.rds")
saveRDS(dat.scripts, "exported_data/dat_scripts.rds")

# Load RDS data 
dat.characters <- readRDS("exported_data/dat_characters.rds") 
dat.episodes <- readRDS("exported_data/dat_episodes.rds") 
dat.locations <- readRDS("exported_data/dat_locations.rds") 
dat.scripts <- readRDS("exported_data/dat_scripts.rds") 







