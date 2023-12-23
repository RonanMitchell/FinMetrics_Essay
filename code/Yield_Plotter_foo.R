Yield_Plotter <- function(Data, Vec, y, title, subtitle, legend){


    {{Data}} %>%

        filter(Name %in% {{Vec}}) %>%

        select(Name, date, {{y}}) %>%

        group_by(Name) %>%

        ggplot() +

        geom_rect(data = HV1,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "blue4",
                  alpha = alpha) +

        geom_rect(data = HV2,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "blue4",
                  alpha = alpha) +

        geom_rect(data = HV5,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "red4",
                  alpha = alpha) +

        geom_rect(data = HV6,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "red4",
                  alpha = alpha) +

        geom_rect(data = HV7,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "red4",
                  alpha = alpha) +

        geom_rect(data = HV8,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = "red4",
                  alpha = alpha) +

        geom_line(aes(x = as.Date(date),
                      y = {{y}},
                      color = Name),
                  linewidth = 0.7,
                  alpha = 1) +

        geom_hline(yintercept = 0) +

        xlab("") + ylab("") +

        labs(title = title,
             subtitle = subtitle) +

        theme_light() +

        theme(legend.position = {{legend}},
              legend.text = element_text(size = rel(0.6)),
              legend.title = element_blank(),
              plot.title = element_text(size = rel(0.8)),
              plot.subtitle = element_text(size = rel(0.7)))


}