Yield_Plotter <- function(Data, Vec, y, title){


    Data %>%

        filter(Name %in% {{Vec}}) %>%

        select(Name, date, {{y}}) %>%

        group_by(Name) %>%

        ggplot(aes(x = date, y = {{y}}, color = Name)) +

        geom_line(linewidth = 0.8,
                  alpha = 0.8) +

        geom_hline(yintercept = 0) +

        xlab("") + ylab("") +

        labs(title = title) +

        #scale_color_brewer(palette = "Set1") + # my favorite

        theme_light() +

        theme(legend.position = "bottom",
              legend.title = element_blank())


}