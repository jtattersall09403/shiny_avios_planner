source('global.R')

# ---- Load routes data ----

# Get airports
airports_df <- read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                          sep = ",",
                          header = FALSE,
                          col.names = c("airport_id",
                                        "airport_name",
                                        "city",
                                        "county",
                                        "ata",
                                        "icao",
                                        "lat",
                                        'lon',
                                        "alt",
                                        'tz',
                                        'dst',
                                        'tz_db',
                                        'type',
                                        'source')) %>%
  as_tibble()
  
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
      left_join(select(airports_df, source_airport_id = airport_id, source_lat = lat, source_lon = lon)) %>%
      left_join(select(airports_df, dest_airport_id = airport_id, dest_lat = lat, dest_lon = lon))
    
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
  

# Filter to oneworld
oneworld_df <- airlines_df %>%
  mutate(airline_name = tolower(airline_name)) %>%
  inner_join(tibble(airline_name = oneworld_airlines))

  
# Oneworld routes
oneworld_routes <- flights_df %>%
      mutate(airline_id = as.integer(airline_id)) %>%
      inner_join(select(oneworld_df, airline_id, airline_name))

# ---- Query charges API ----

# Take a sample of OneWorld routes to query
if (!file.exists('data/oneworld_routes_sample.rds')) {
  
  # Take a sample
  oneworld_routes_sample <- oneworld_routes %>%
    sample_n(2000)
  
  # Save it
  saveRDS(oneworld_routes_sample, 'data/oneworld_routes_sample.rds')
  
} else {
  oneworld_routes_sample <- readRDS('data/oneworld_routes_sample.rds')
} 

# Initialise results object
route_charges <- c() 

# Create access token
access_token <- get_token()

# Loop through it and query. Don't use apply, as you want to save as you go.
for (i in (length(route_charges)+1):nrow(oneworld_routes_sample)) {
  
  # Progress report
  if (i %% 50 == 0) {
    message(paste0("Routes checked: ", i, ". Proportion available: ", round(mean(!is.na(route_charges)), 3)))
    saveRDS(route_charges, 'data/route_charges.rds')
  }
  
  # Get charges
  route_charges[i] <- get_fees(access_token = access_token, 
                               origin = oneworld_routes_sample$source_airport[i], 
                               dest = oneworld_routes_sample$dest_airport[i],
                               dep_date = "2023-11-01", 
                               max_num_flights = 1,
                               airline_code = oneworld_routes_sample$airline[i], 
                               travel_class = "BUSINESS")
  
}

# Add to data
oneworld_routes_sample_2 <- oneworld_routes_sample %>%
  slice(1:length(route_charges)) %>%
  mutate(charges = route_charges) %>%
  filter(!is.na(charges))

# Save
saveRDS(oneworld_routes_sample_2, 'data/oneworld_routes_sample_with_charges.rds')

# ---- Iteration 2: further in future ----

# Take a sample, not including routes you already have some data for
oneworld_routes_sample <- oneworld_routes %>%
  anti_join(oneworld_routes_sample_2) %>%
  sample_n(900)

# Save it
saveRDS(oneworld_routes_sample, 'data/oneworld_routes_sample_iteration_2.rds')

# Initialise results object
route_charges <- c() 

# Create access token
access_token <- get_token()

# Loop through it and query. Don't use apply, as you want to save as you go.
for (i in (length(route_charges)+1):nrow(oneworld_routes_sample)) {
  
  # Progress report
  if (i == 2 | i %% 50 == 0) {
    message(paste0("Routes checked: ", i, ". Proportion available: ", round(mean(!is.na(route_charges)), 3)))
    saveRDS(route_charges, 'data/route_charges_2.rds')
  }
  
  # Get charges
  route_charges[i] <- get_fees(access_token = access_token, 
                               origin = oneworld_routes_sample$source_airport[i], 
                               dest = oneworld_routes_sample$dest_airport[i],
                               dep_date = "2023-09-01", 
                               max_num_flights = 1,
                               airline_code = paste(unique(oneworld_routes_sample$airline), collapse = ","), 
                               travel_class = "BUSINESS")
  
}

# Add to data
oneworld_routes_sample_2 <- oneworld_routes_sample %>%
  slice(1:length(route_charges)) %>%
  mutate(charges = route_charges) %>%
  filter(!is.na(charges))

# Save
saveRDS(oneworld_routes_sample_2, 'data/oneworld_routes_sample_with_charges_2.rds')

# Combined
sample_1 <- readRDS('data/oneworld_routes_sample_with_charges.rds')
sample_2 <- readRDS('data/oneworld_routes_sample_with_charges_2.rds')
combined <- sample_1 %>% bind_rows(sample_2)

# Save
saveRDS(combined, 'data/oneworld_combined_sample_with_charges.rds')
