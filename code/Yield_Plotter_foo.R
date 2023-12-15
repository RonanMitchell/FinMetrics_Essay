Yield_Plotter <- function(Data, Vec, y, title, subtitle, legend){


    Data %>%

        filter(Name %in% {{Vec}}) %>%

        select(Name, date, {{y}}) %>%

        group_by(Name) %>%

        ggplot(aes(x = as.Date(date), y = {{y}}, color = Name)) +

        geom_line(linewidth = 0.8,
                  alpha = 0.8) +

        geom_hline(yintercept = 0) +

        xlab("") + ylab("") +

        labs(title = title,
             subtitle = subtitle) +

        theme_light() +

        theme(legend.position = {{legend}},
              legend.title = element_blank(),
              plot.title = element_text(size = rel(0.8)),
              plot.subtitle = element_text(size = rel(0.7)))


}