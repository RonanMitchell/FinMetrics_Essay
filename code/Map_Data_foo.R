Map_Data_foo <- function(data, y){

    # Load the map data

    world_map <-

        ne_countries(scale = "medium",
                     returnclass = "sf") %>% # make sf

        filter(admin != "Antarctica") # do not include antarctica

    # Rename according to names in the world map set

    BondsRenamed <-

        {{data}} %>%

        filter(Name != "EURO") %>% # this is a zone, not a country

        mutate(name = ifelse(Name == "CHINA", "China",

                      ifelse(Name == "NZ", "New Zealand",

                      ifelse(Name == "UK", "United Kingdom",

                      ifelse(Name == "US", "United States",

                      ifelse(Name == "ZA", "South Africa",

                      Name)))))) %>%

        group_by(name) %>%

        summarize(mean_yield = mean({{y}}, na.rm = TRUE)) %>% # or mean whatevs

        mutate(mean_yield = min_rank(mean_yield)) # rank by countries, otherwise

    # outliers in levels (such as Venezuela) are so skewed that the graph becomes
    # meaningless.

    ###

    Merged <- left_join(world_map,

                        BondsRenamed,

                        by = c("name"))

    return(Merged)

}