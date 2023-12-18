High_Vol_foo <- function(data, x, quantile, type){


High_Vol <-

   {{data}} %>%

    mutate(SemiAnnual = paste0(format(date, "%Y"),

                        "",

                        ifelse(

                        month(date) %in% c(1, 2, 3, 4, 5, 6),

                        "-01-01", "-07-01")))



    if ({{type}} == "sd") {


            Result <-

                High_Vol %>%

                group_by(SemiAnnual) %>%

                summarise(SD = sd({{x}}) * sqrt(6)) %>%

                mutate(TopQtile = quantile(SD, {{quantile}})) %>%

                filter(SD > TopQtile) %>%

                pull(SemiAnnual)


        } else {


            Result <-

                High_Vol %>%

                group_by(SemiAnnual) %>%

                summarise(SD = mean({{x}})) %>%

                mutate(TopQtile = quantile(SD, {{quantile}})) %>%

                filter(SD > TopQtile) %>%

                pull(SemiAnnual)


        }


return(Result)


}