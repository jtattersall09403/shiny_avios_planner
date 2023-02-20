# Load functions
library(dplyr)
library(tidyr)
library(readr)
library(igraph)
library(geosphere)
library(tidymodels)
library(bundle)
library(stringr)

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
                       "Air Pacific", # this is Fiji Airways
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
# mod_bundle <- readRDS("models/xgb_model.rds")
# xgb_model <- unbundle(mod_bundle)
conf_ints <- readRDS('models/xgb_conf_ints.rds')

# Load functions and data
sapply(list.files('R/', full.names = TRUE), source)
source('R scripts/load_data.R')

# Example route
twostop <- get_routes_miles("LHR", "DPS", positioning_radius = 1500, max_stops = 2)
twostop %>% mutate(total_cost_eq = 0.01 * avios_biz + charge_central) %>% arrange(total_cost_eq) %>% View
onestop <- get_routes_miles("LAD", "LHR", positioning_radius = 1150, max_stops = 1)

twostop %>% filter(booking_type == "multi-partner") %>% filter(total_dist < 10000 - min(onestop$total_dist))
onestop %>% arrange(total_dist)
onestop %>% filter(grepl("LHR", route)) %>% arrange(charge_low) %>% filter(total_dist < 10000 - 4573)
twostop %>% filter(charge_high < 300) %>% arrange(avios_biz)
onestop %>% filter(charge_high < 300) %>% arrange(avios_biz)