#' Visualize mean flight delays by airport
#'
#' Creates a plot of the mean arrival delay for flights from New York
#' to each destination airport, placed by longitude and latitude.
#' Data comes from the nycflights13 package.
#'
#' @return A ggplot object showing one point per airport, colored by
#'   mean arrival delay in minutes.
#'@importFrom dplyr select group_by filter summarise inner_join
#'@importFrom ggplot2 ggplot geom_point labs aes
#'@importFrom rlang .data
#' @examples
#' visualize_airport_delays()
#'@export
visualize_airport_delays <- function(){

  airports_location <- nycflights13::airports |>
    select("faa", "lat", "lon")

  avg_flight_delay <- nycflights13::flights |>
    select("arr_delay", "dest") |>
    group_by(.data$dest) |>
    summarise(avg_delay = mean(.data$arr_delay, na.rm = TRUE)) |>
    filter(!is.na(.data$avg_delay))

  combined_data <- inner_join(avg_flight_delay, airports_location, by = c("dest" = "faa")) |>
      select(!"dest")

  delay_plot <- ggplot(combined_data, aes(x = .data$lon, y = .data$lat, color = .data$avg_delay )) +
    geom_point(size = 4)+
    labs(x = "Longitude", y = "Latitude", title = "Mean arrival delay per airport")+
    theme_liu()
  delay_plot

}




