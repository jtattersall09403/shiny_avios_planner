get_routes <- function(flights_graph, from, to, max_stops = 2) {

  # If more than one possible start point: simple paths can have multiple end points but not multiple starts
  # So invert it and do method = "in"
  route_paths <- igraph::all_simple_paths(flights_graph,
                                            from = to,
                                            to = from,
                                            mode = "in", 
                                            cutoff = max_stops + 1)
  
  
  # Format
  tidy_routes <- lapply(route_paths, function(x) {
    tibble(from = names(x)) %>%
      arrange(desc(row_number())) %>%
      mutate(to = lead(from),
             idx = row_number(),
             route = paste(rev(names(x)), collapse = "-"))
  }) %>% bind_rows() %>% filter(!is.na(to))
}
