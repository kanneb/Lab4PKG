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
#'
#' @examples
#' visualize_airport_delays()
#'@export
visualize_airport_delays <- function(){

  airports_location <- nycflights13::airports |>
    select(faa, lat, lon)

  avg_flight_delay <- nycflights13::flights |>
    select(arr_delay, dest) |>
    group_by(dest) |>
    summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
    filter(!is.na(avg_delay))

  combined_data <- inner_join(avg_flight_delay, airports_location, by = c("dest" = "faa")) |>
      select(!dest)

  plot <- ggplot(combined_data, aes(x = lon, y = lat, color = avg_delay )) +
    geom_point(size = 4)+
    labs(x = "Longitude", y = "Latitude", title = "Mean arrival delay per airport")+
    theme_liu()
  plot

}




