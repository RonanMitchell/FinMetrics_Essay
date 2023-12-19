Spread_Comparison_foo <- function(data,
                                  column,
                                  column_name,
                                  Vec,
                                  colours,
                                  title){

    {{data}} %>%

        select(date, Name, {{column}}) %>%

        spread(key = Name, value = {{column}}) %>%

        mutate(across(-c(date, "US"), ~ . - US)) %>%

        mutate(US = 0) %>%

        pivot_longer(cols = -date,
                     names_to = "Name",
                     values_to = column_name)  %>%

        arrange(Name) %>%

        filter(Name %in% {{Vec}}) %>%

        group_by(Name) %>%

        # This is the main point of the function, to plot the yield:

        ggplot() +

        geom_ribbon(

            data = . %>%

                group_by(date) %>%

                summarise(ymin = min({{column}}),
                          ymax = max({{column}})),

            aes(x = as.Date(date), ymin = ymin, ymax = ymax),
            fill = "grey40",
            alpha = 0.3) +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = 0,
                 ymax = Inf,
                 fill = "red4",
                 alpha = 0.2) +

        annotate("rect",
                 xmin = as.Date("2010-01-01"),
                 xmax = as.Date("2022-06-01"),
                 ymin = -Inf,
                 ymax = 0,
                 fill = "blue4",
                 alpha = 0.2) +

        geom_line(aes(x = as.Date(date),
                      y = {{column}},
                      color = Name),
                  linewidth = 0.8,
                  alpha = 1) +

        geom_hline(yintercept = 0) +

        xlab("") + ylab("") +

        labs(title = title,
             subtitle = "") +

        scale_color_manual(values = colours) +

        theme_light() +

        theme(legend.position = "bottom",
              legend.text = element_text(size = rel(0.6)),
              legend.title = element_blank(),
              plot.title = element_text(size = rel(0.8)),
              plot.subtitle = element_text(size = rel(0.7)))

}