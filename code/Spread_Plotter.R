Spread_Plotter <- function(data,
                           column,
                           Vec,
                           column_name,
                           title,
                           subtitle,
                           legend){

# The following code is a terrible way to simply subtract the US bond Yield
# from other yields. This works instead:

    # mutate(BondYield_10 = (BondYield_10 - BondYield_10[Name == "US"]))

# The reason I did it in this very manual way is because it is inside a
# function, and the tidy notation sometimes required for functions is very
# tedious, such as !!{{y}} notation etc...

    {{data}} %>%

        select(date, Name, {{column}}) %>%

        spread(key = Name, value = {{column}}) %>%

        mutate(across(-c(date, "US"), ~ . - US)) %>%

        mutate(US = 0) %>%

        pivot_longer(cols = -date,
                     names_to = "Name",
                     values_to = {{column_name}})  %>%

        arrange(Name) %>%

        filter(Name %in% {{Vec}}) %>%

        group_by(Name) %>%

# This is the main point of the function, to plot the yield:

        ggplot() +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = 0,
                 ymax = Inf,
                 fill = "red4",
                 alpha = 0.15) +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = -Inf,
                 ymax = 0,
                 fill = "blue4",
                 alpha = 0.15) +

        geom_line(aes(x = as.Date(date),
                      y = {{column}},
                      color = Name),
                  linewidth = 0.85,
                  alpha = 1) +

        geom_hline(yintercept = 0) +

        xlab("") + ylab("") +

        labs(title = title,
             subtitle = subtitle) +

        theme_light() +

        theme(legend.position = legend,
              legend.text = element_text(size = rel(0.6)),
              legend.title = element_blank(),
              plot.title = element_text(size = rel(0.8)),
              plot.subtitle = element_text(size = rel(0.7)))

}