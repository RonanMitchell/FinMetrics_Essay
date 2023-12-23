Ridge_Plotter <- function(Data, name, var, countries, title){


    mean_yield <-

        {{Data}} %>%

        filter(Name == {{name}}) %>%

        summarize(mean_yield = mean({{var}})) %>%

        pull(mean_yield)


    {{Data}} %>%

        filter(Name %in% {{countries}}) %>%

        ggplot(aes(x = as.numeric({{var}}),
                   y = Name,
                   fill = factor(Name))) +

        theme_minimal() +

        geom_density_ridges(scale = 2,
                            rel_min_height = 0.01,
                            alpha = 0.5) +

        geom_vline(xintercept = mean_yield,
                   linetype = "dashed",
                   color = "grey20") +

        labs(title = title) +
        xlab("") +
        ylab("") +

        theme(legend.position = "none",
              axis.text.x = element_blank(),
              plot.title = element_text(size = rel(0.8)))

}