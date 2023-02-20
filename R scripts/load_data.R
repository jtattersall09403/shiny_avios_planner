# Get airports data
# Continent lookup
continent_df <- read_csv('data/continent_lookup.csv') %>% mutate(continent = ifelse(is.na(continent), "NAm", continent))
airports_df <- read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                          sep = ",",
                          header = FALSE,
                          col.names = c("airport_id",
                                        "airport_name",
                                        "city",
                                        "country",
                                        "iata",
                                        "icao",
                                        "lat",
                                        'lon',
                                        "alt",
                                        'tz',
                                        'dst',
                                        'tz_db',
                                        'type',
                                        'source')) %>%
  as_tibble() %>%
  left_join(continent_df)

# Flights data
flights_df <- read.delim('https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat', 
                         sep = ",", 
                         header = FALSE, 
                         col.names = c("airline",
                                       "airline_id",
                                       "source_airport",
                                       "source_airport_id",
                                       "dest_airport",
                                       "dest_airport_id",
                                       "codeshare",
                                       "stops",
                                       "equipment")) %>%
  as_tibble() %>%
  mutate(source_airport_id = as.integer(source_airport_id),
         dest_airport_id = as.integer(dest_airport_id)) %>%
  left_join(select(airports_df, source_continent = continent, source_country = country, source_airport = iata, source_lat = lat, source_lon = lon)) %>%
  left_join(select(airports_df, dest_continent = continent, dest_country = country, dest_airport = iata, dest_lat = lat, dest_lon = lon)) %>%
  mutate(international = dest_country != source_country,
         across(.cols = c(source_airport, source_country, dest_country, airline), .fns = as.factor))

# Get great circle cistances
gc_distances <- distHaversine(p1 = as.matrix(select(flights_df, source_lon, source_lat)),
                              p2 = as.matrix(select(flights_df, dest_lon, dest_lat)))

# Add to data
flights_df <- flights_df %>%
  mutate(gc_distance = gc_distances/1609.344) # convert to miles

# Get airlines data
airlines_df <- read.delim('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat',
                            sep = ",",
                            header = FALSE,
                            col.names = c("airline_id",
                                          "airline_name",
                                          "alias",
                                          "iata",
                                          "icao",
                                          "callsign",
                                          "country",
                                          "active")) %>%
    as_tibble()

# Get Avios airlines

# Filter to oneworld
oneworld_df <- airlines_df %>%
  mutate(airline_name = tolower(airline_name),
         airline_id = as.character(airline_id)) %>%
  inner_join(tibble(airline_name = oneworld_airlines))

# Load charges
# Manula corrections
# EZE - MAD on IB is 770 in biz and 360 in econ.
charges_lookup <- readRDS('data/oneworld_combined_sample_with_charges.rds')

# Add known charges to data
oneworld_routes <- flights_df %>% 
  inner_join(select(oneworld_df, airline_id, airline_name), by = "airline_id") %>%
  left_join(select(charges_lookup, airline, source_airport, dest_airport, charges)) 

# Create fee predictions where missing
# pred_charges <- if (exists("xgb_model")) {
#   predict(xgb_model, new_data = oneworld_routes)
# } else {
#   mean(oneworld_routes$charges, na.rm = T)
# }

# Add to data, and apply heuristics of various kinds. Remove codeshares.
oneworld_routes <- oneworld_routes %>%
  group_by(airline_name, source_continent) %>%
  mutate(pred_charge = mean(charges, na.rm = T)) %>%
  group_by(airline_name) %>%
  mutate(pred_charge = ifelse(is.na(pred_charge),
                              mean(charges, na.rm = T),
                              pred_charge)) %>%
  mutate(#pred_charge = pred_charges$.pred,
         pred_charge_low = pred_charge * (1 + conf_ints[1]),
         pred_charge_high = pred_charge * (1 + conf_ints[2]),
         charge_estimate = ifelse(is.na(charges), pred_charge, charges)) %>%
  filter(codeshare != "Y") %>%
  mutate(
    charge_rule = case_when(
      # Minimum
      charge_estimate < min(charges, na.rm = TRUE) ~ min(charges, na.rm = TRUE),
      # Reward flight saver
      airline == "BA" & gc_distance < 2000 ~ 25,
      # Domestic US $5.60
      source_country == "United States" & dest_country == "United States" ~ 5,
      # Domestic Australia on Qantas
      airline_name == "qantas" & source_country == "Australia" & dest_country == "Australia" ~ 22,
      # Domestic Japan on JAL
      airline == "JL" & source_country == "Japan" & dest_country == "Japan" ~ 2.5,
      # Else
      TRUE ~ as.numeric(NA)
    )
  )  %>%
  mutate(
    charge_central = case_when(
      !is.na(charge_rule) ~ charge_rule,
      !is.na(charges) ~ charges,
      TRUE ~ pred_charge
    ),
    charge_low = case_when(
      !is.na(charge_rule) ~ charge_rule,
      !is.na(charges) ~ charges,
      TRUE ~ pred_charge_low
    ),
    charge_high = case_when(
      !is.na(charge_rule) ~ charge_rule,
      !is.na(charges) ~ charges,
      TRUE ~ pred_charge_high
    )
  ) %>% ungroup()

# Add avios cost. Remove Russia!
oneworld_routes <- oneworld_routes %>%
  mutate(dummy=TRUE) %>%
  left_join(ba_df %>% mutate(dummy=TRUE)) %>%
  filter(gc_distance >= ba_miles_from, gc_distance < ba_miles_to) %>%
  left_join(p_df %>% mutate(dummy=TRUE)) %>%
  filter(gc_distance >= p_miles_from, gc_distance < p_miles_to) %>%
  mutate(avios_econ = ifelse(airline == "BA", ba_avios_econ, p_avios_econ),
         avios_biz = ifelse(airline == "BA", ba_avios_biz, p_avios_biz)) %>%
  select(-contains("miles_from"), -contains("miles_to"), -ba_avios_econ, -ba_avios_biz, -p_avios_econ, -p_avios_biz)

# Russia
oneworld_routes <- oneworld_routes %>% filter(source_country != "Russia", dest_country != "Russia")

# Create igraph

# Create nodes and edges
edges <- oneworld_routes %>% ungroup %>% select(from = source_airport, to = dest_airport)
flights_graph <- graph_from_edgelist(as.matrix(edges))
