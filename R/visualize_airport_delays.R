#' Visualize Average Flight Delays by Airport Location
#'
#' This function creates a scatter plot visualizing the average flight delays
#' for different airports using the `nycflights13` dataset. The average delay 
#' is calculated by taking the mean of the arrival and departure delays. 
#' The resulting plot displays the airports' geographical locations, with 
#' point sizes and colors indicating the average delay times.
#'
#' @return A ggplot2 object representing the scatter plot of average flight delays.
#' 
#' @import ggplot2
#' @import dplyr
#' @import nycflights13
#'
#' @examples
#' library(LRpkg)
#' library(nycflights13)
#' visualize_airport_delays()
#'
#' @export
visualize_airport_delays <- function() {
  delay_data <- flights %>%
    mutate(delay = (arr_delay + dep_delay) / 2) %>%
    filter(!is.na(delay)) %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(delay, na.rm = TRUE)) 
  
  airport_delay_data <- delay_data %>%
    inner_join(airports, by = c("dest" = "faa"))
  
  ggplot(airport_delay_data, aes(x = lon, y = lat)) +
    geom_point(aes(size = mean_delay, color = mean_delay), alpha = 0.9) +
    scale_size(range = c(3, 10)) +
    scale_color_viridis_c(option = "F") +
    labs(title = "Average Flight Delay by Airport Location",
         x = "Longitude",
         y = "Latitude",
         size = "Average Delay",
         color = "Average Delay") +
    guides(size = guide_legend(override.aes = list(color = "white"))) +
    theme_liu()
}
