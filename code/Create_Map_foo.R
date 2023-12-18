# function

# Define a function to create a choropleth map

create_map <- function(df, title, legend) {

    # Compute the representative points for each country

    points <- st_point_on_surface(st_geometry(df))

    # Add the longitude and latitude of the representative points to the data frame

    df$lon <- st_coordinates(points)[,1]
    df$lat <- st_coordinates(points)[,2]

    # Create the plot

ggplot(data = df) +

    geom_sf(aes(fill = mean_yield)) +

    scale_fill_gradientn(colors = c("blue",
                                        "deeppink"),
                         na.value = "grey80",
                         name = "",
                         limits = c(1, 26)) +

    xlab("") + ylab("") +

    theme_classic() +

    theme(plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = legend,
          legend.background = element_rect(fill = "black"),
          legend.text = element_text(color = "white"),
          plot.margin = margin(0, 0, 0, 0),
          plot.title = element_text(color = "white",
                                    size = rel(0.8))) +

    labs(title = title)


}
