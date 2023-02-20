library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(bs4Dash)
library(igraph)
library(geosphere)
library(tidymodels)
library(bundle)
library(stringr)

sapply(list.files('R', full.names = TRUE), source)

# Oneworld airlines
oneworld_airlines <- c("Alaska Airlines",
                       "American Airlines",
                       "British Airways", 
                       "Cathay Pacific", 
                       "Finnair",
                       "Iberia Airlines",
                       "Iberia Express",
                       "Air Nostrum",
                       "Japan Airlines",
                       "Air Pacific", # Fiji Airways
                       "Malaysia Airlines",
                       "Qantas",
                       "QantasLink",
                       "Jetconnect",
                       "Qatar Airways",
                       "Royal Jordanian",
                       "Royal Air Maroc",
                       "SriLankan Airlines") %>% tolower

# Read avios lookups
multi_partner_df <- read_csv('data/multi_partner_chart.csv')
ba_df <- read_csv('data/ba_avios_chart.csv')
p_df <- read_csv('data/partner_avios_chart.csv')

# Load model
mod_bundle <- readRDS("models/xgb_model.rds")
xgb_model <- unbundle(mod_bundle)
conf_ints <- readRDS('models/xgb_conf_ints.rds')
