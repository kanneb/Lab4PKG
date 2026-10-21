#' Visualizes flight delays
#'
#' Visualizes the mean delay of flights for different airports
#' by longitude and latitude.
#'
#'
#'@importFrom nycflights13 airports flights
#'@importFrom dplyr
#'@importFrom ggplot2
#'
#'@export
library(ggplot2)
visualize_airport_delays <- function(){

  airports <- airports |>
    select(faa, lat, lon) |>
    distinct(lon, lat, .keep_all = TRUE)
  airports

  avg_flight_delay <- flights |>
    select(arr_delay, dest) |>
    group_by(dest) |>
    summarise(avg_delay = mean(arr_delay, na.rm = TRUE))
  avg_flight_delay

  combined_data <- left_join(avg_flight_delay, airports, by = c("dest" = "faa")) |>
      select(!dest)

  plot <- ggplot(combined_data, aes(x = lon, y = lat, color = avg_delay )) +
    geom_point()+
    labs(x = "Longitude", y = "Latitude", title = "Mean delay per airport")
  plot

}

visualize_airport_delays()


