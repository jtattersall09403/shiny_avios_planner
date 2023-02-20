get_routes_miles <- function(from, to, positioning_radius = 0, max_stops = 2) {
  
  # Stop if likely to take too long
  if (max_stops > 3) {
    warning("Max stops greater than 3 will be very slow to run. Re-run with max_stops <= 3.")
    stop()
  }
  
  if (positioning_radius > 1500 & max_stops > 1) {
    warning("With a large positioning radius, many starting airports are considered. max_stops > 1 will be too slow to run. Re-run with max_stops <= 1.")
    stop()
  }
  
  # Get reactive data
  # flights_graph <- get_route_graph()
  # flights_df <- get_flights_data()
  # airlines_df <- get_airlines_data()
  # oneworld_routes <- get_oneworld_routes()
  
  # Get all possible starting airports
  starting_airports <- flights_df %>%
    filter(source_airport == from,
           gc_distance < positioning_radius) %>%
    inner_join(distinct(oneworld_routes, dest_airport = source_airport)) %>%
    pull(dest_airport)
  
  # Get routes
  all_routes <- get_routes(flights_graph, from = c(from, starting_airports), to = to, max_stops = max_stops)
  
  # Format
  formatted_routes <- all_routes %>%
    left_join(select(oneworld_routes, 
                     from = source_airport, 
                     to = dest_airport,
                     gc_distance,
                     airline,
                     charge_low,
                     charge_high,
                     charge_central,
                     avios_econ,
                     avios_biz)) %>%
    left_join(select(airlines_df, airline=iata, airline_name))
  
  # Route-level charges and avios cost. Get cheapest airline option for each segment
  segment_routes <- formatted_routes %>%
    group_by(route, idx) %>%
    filter(row_number(charge_central) == 1) %>%
    ungroup %>%
    group_by(route) %>%
    summarise(airlines = paste(airline_name, collapse = "; "),
              total_dist = sum(gc_distance),
              across(.cols = c(avios_econ, avios_biz, charge_low, charge_central, charge_high), .fns = sum)) %>%
    arrange(charge_central) %>%
    ungroup %>%
    mutate(booking_type = "segment-by-segment")
  
  # Multi-partner awards
  mp_routes <- formatted_routes %>%
    group_by(route, from, to, idx, gc_distance,
             charge_low,
             charge_high,
             charge_central) %>%
    summarise(airline_options = paste(airline_name, collapse = ", ")) %>%
    group_by(route) %>%
    mutate(total_dist = sum(gc_distance)) %>%
    ungroup %>%
    arrange(total_dist, route, idx) %>%
    mutate(partner_option = grepl(",", airline_options) | airline_options != "British Airways") %>%
    group_by(route) %>%
    mutate(total_partner_segments = sum(partner_option)) %>%
    mutate(dummy=TRUE) %>%
    left_join(multi_partner_df %>% mutate(dummy=TRUE)) %>%
    filter(total_dist >= mp_miles_from, total_dist < mp_miles_to) %>%
    select(-dummy) %>%
    group_by(route, total_dist, mp_miles_to, total_partner_segments, avios_econ = mp_avios_econ, avios_biz = mp_avios_biz) %>%
    summarise(airlines = paste(airline_options, collapse = "; "),
              across(.cols = c(charge_low, charge_central, charge_high), .fns = sum)) %>%
    arrange(avios_biz, charge_central) %>%
    filter(total_partner_segments >= 2) %>%
    ungroup %>%
    mutate(booking_type = "multi-partner")
  
  # Combine into single list
  bind_rows(list(segment_routes, mp_routes)) %>%
    arrange(charge_central, avios_biz) %>%
    select(booking_type, route, airlines, charge_low, charge_central, charge_high, avios_econ, avios_biz, total_dist)
  
}