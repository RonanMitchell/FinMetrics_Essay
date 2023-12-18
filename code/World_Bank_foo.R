World_Bank_foo <- function(data, Vec, target_column) {

    transformed_data <- data %>%

        `colnames<-`(sub("^X", "", colnames(.))) %>%

        mutate(
            Name = case_when(

                Name == "China" ~ "CHINA",
                Name == "New Zealand" ~ "NZ",
                Name == "United Kingdom" ~ "UK",
                Name == "United States" ~ "US",
                Name == "Russian Federation" ~ "Russia",
                Name == "Venezuela, RB" ~ "Venezuela",
                Name == "South Africa" ~ "ZA",

                TRUE ~ as.character(Name))) %>%

        filter(Name %in% {{Vec}}) %>%

        gather(date, {{ target_column }}, -Name) %>%

        mutate(date = as.Date(paste0(date, "-01-01")))

    return(transformed_data)

}