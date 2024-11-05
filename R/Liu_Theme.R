#' Liu Theme for ggplot2
#'
#' This function creates a custom theme for ggplot2 visualizations with a
#' blue background and white text elements. The theme is designed to enhance
#' the visual appeal and readability of plots, particularly when representing
#' data related to flight delays or any other data visualizations that benefit
#' from a blue color scheme.
#'
#' @return A ggplot2 theme object that can be added to ggplot2 plots.
#' @export
theme_liu <- function() {
  
  liu_colors <- list(
    primary_blue = "#0bbcec",
    secondary_white = "#ffffff"
  )
  
  theme(
    plot.background = element_rect(fill = liu_colors$primary_blue, color = NA),
    plot.title = element_text(face = "bold", color = liu_colors$secondary_white, size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", color = liu_colors$secondary_white, size = 12, hjust = 0.5),
    
    panel.background = element_rect(fill = liu_colors$primary_blue, color = "white"),
    panel.grid.major = element_line(color = liu_colors$secondary_white), 
    panel.grid.minor = element_blank(), 
    
    axis.line = element_line(color = liu_colors$secondary_white),
    axis.text = element_text(color = liu_colors$secondary_white),
    axis.title = element_text(color = liu_colors$secondary_white, size = 14, face = "bold"),
    axis.ticks = element_blank(),
    
    legend.background = element_rect(fill = liu_colors$primary_blue, color = "white"),
    legend.title = element_text(face = "bold", color = liu_colors$secondary_white),
    legend.text = element_text(color = liu_colors$secondary_white), 
    
    plot.caption = element_text(color = liu_colors$primary_blue, size = 10, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  )
}
