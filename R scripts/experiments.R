# Load functions
sapply(list.files('R/', full.names = TRUE), source)

# Example route for a Maldives luxury trip!
twostop <- get_routes_miles("LHR", "LPB", positioning_radius = 1500, max_stops = 2)
twostop %>% mutate(total_cost_eq = 0.01 * avios_biz + charge_central) %>% arrange(total_cost_eq) %>% View
onestop <- get_routes_miles("LAD", "LHR", positioning_radius = 1150, max_stops = 1)

twostop %>% filter(booking_type == "multi-partner") %>% filter(total_dist < 10000 - min(onestop$total_dist))
onestop %>% arrange(total_dist)
onestop %>% filter(grepl("LHR", route)) %>% arrange(charge_low) %>% filter(total_dist < 10000 - 4573)
twostop %>% filter(charge_high < 300) %>% arrange(avios_biz)
onestop %>% filter(charge_high < 300) %>% arrange(avios_biz)

# Positioning flight to Rome
# FCO - DOH - MLE (Qatar Airways, Business)
# MLE - DOH - LHR (Qatar Airways, Business)
# Total distance 4573 + 5321 = 9894 miles
# Total Avios per person (multi-partner chart) = 140,000
# TOtal cash cost ~= 297 + 511 to 454 + 575 = 808 to 1029

# Or ~£400 and 116k for biz outbound and econ return; FCO - CMB - MLE return. Rome, Sri Lanka and Maldives in one trip!

# What about South America
twostop <- get_routes_miles("LHR", "EZE", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "EZE", positioning_radius = 1150, max_stops = 1)

twostop
onestop

twostop %>% filter(booking_type == "multi-partner") %>% filter(total_dist < 10000 - min(onestop$total_dist))
onestop %>% arrange(total_dist)
onestop %>% filter(grepl("LHR", route)) %>% arrange(charge_low) %>% filter(total_dist < 10000 - 4573)
twostop %>% filter(charge_high < 300) %>% arrange(avios_biz)
onestop %>% filter(charge_high < 300) %>% arrange(avios_biz)

# Options
# Lisbon, Casablanca, Sao Paulo, Buenos Aires: ~£450 + 106k biz (Royal Air Maroc; Royal Air Maroc; Qatar Airways)
# Buenos Aires - Madrid: £360 ~ 31k econ (Iberia)

# What about Perth
twostop <- get_routes_miles("LHR", "PER", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "PER", positioning_radius = 1150, max_stops = 1)

twostop
twostop %>% arrange(avios_biz, charge_low) %>% View
onestop


# Options
# Outbound: Helsinki, Bankok, Singapore, Perth: ~£250 + 120k biz (multi-partner)
# Or Helsinki, Delhi, Kuala Lumpur, Perth: ~£170 ~ 120k biz
# Helsinki, Xi'an, Hong Kong, Perth: ~£250
# Return: Helsinki, Singapore, Perth (~£200 + 44k econ)

# Or Japan
twostop <- get_routes_miles("LHR", "NRT", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "NRT", positioning_radius = 1150, max_stops = 1)

twostop
twostop %>% arrange(avios_biz, charge_low) %>% View
onestop

# Or US/Asia and Hawaii
twostop <- get_routes_miles("LHR", "HNL", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "HNL", positioning_radius = 1150, max_stops = 1)

twostop
twostop %>% arrange(avios_biz, charge_low) %>% View
onestop %>% arrange(total_dist)
twostop %>% filter(total_dist < 20000 - 8697) %>% mutate(stops = str_count(route, "-") - 1) %>% arrange(charge_central, stops) %>% View

# Round the world for 200k avios in biz: Helsinki, Tokyo, Hawaii, then two US cities, eg.
# LA, Dallas, Portland, San Diego, Seattle; then New York/Charlotte/Philadelphia

# Or Canada
twostop <- get_routes_miles("LHR", "YVR", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "YVR", positioning_radius = 1150, max_stops = 1)

twostop %>% arrange(avios_biz) %>% filter(charge_central < 300)
twostop %>% arrange(avios_biz, charge_low) %>% View
onestop


# Or Pacific
twostop <- get_routes_miles("LHR", "SUV", positioning_radius = 1150, max_stops = 2)
onestop <- get_routes_miles("LHR", "SUV", positioning_radius = 1150, max_stops = 1)

twostop %>% arrange(avios_biz) %>% filter(charge_central < 300)
twostop %>% arrange(avios_biz, charge_low) %>% View
onestop
