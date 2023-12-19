Spread_Plotter <- function(data,
                           column,
                           Vec,
                           column_name,
                           title,
                           subtitle,
                           legend){

# the below is a terrible way to subtract US yield from all other yields at
# all dates. But, when creating a function, some code does not work. If your
# data is already in tidy format, you can:

    # mutate(Bond_Yield = Bond_Yield - Bond_Yield[Name == "US"])

# but this is insanely difficult to get to work in a function, because you need
# to get used to !! or enquo() notation.

CI_data <-

    {{data}} %>%

    select(date, Name, {{column}}) %>%

    spread(key = Name, value = {{column}}) %>%

    mutate(across(-c(date, "US"), ~ . - US)) %>%

    mutate(US = 0) %>%

    pivot_longer(cols = -date,
                 names_to = "Name",
                 values_to = {{column_name}})  %>%

    filter(Name %in% {{Vec}}) %>%

    na.omit() %>%

    group_by(date) %>%

    summarise(

         CI_lower =

             mean({{column}}) - qt(0.15/2, n() - 1)*sd({{column}})/sqrt(n()),

         CI_upper =

             mean({{column}}) + qt(0.15/2, n() - 1)*sd({{column}})/sqrt(n()))

### ### ### ###

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

        geom_ribbon(data = CI_data,
                    aes(x = date,
                        ymin = CI_lower,
                        ymax = CI_upper),
                    alpha = 0.18) +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = 0,
                 ymax = Inf,
                 fill = "red4",
                 alpha = 0.18) +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = -Inf,
                 ymax = 0,
                 fill = "blue4",
                 alpha = 0.18) +

        geom_line(aes(x = as.Date(date),
                      y = {{column}},
                      color = Name),
                  linewidth = 0.9,
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